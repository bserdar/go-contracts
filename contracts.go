package main

import (
	"fmt"
	"log"
)

/*

This proposal is strictly about the contracts part of the generics
proposal. It describes a way to specify contracts using existing
types, it does not add antyhing to the parameterized types discussion.

Let's first describe parameterized types:


*/

// A Type has a name, and type parameters
type Type interface {
	TypeName() string
	Parameters() TypeArgList
	// Instantiate a parameterized type using type assignments in scope
	InstantiateWithScope(*Scope) (Type, error)
}

func IsParameterized(t Type) bool {
	if _, ok := t.(TypeArgReference); ok {
		return true
	}
	return len(t.Parameters()) != 0
}

/*

  The following are types defined by the language

*/

// Primitive is for int, string, etc. It cannot be parameterized
type Primitive struct {
	Name string
}

func (p Primitive) TypeName() string                          { return p.Name }
func (p Primitive) Parameters() TypeArgList                   { return nil }
func (p Primitive) InstantiateWithScope(*Scope) (Type, error) { return p, nil }

var Int64 = Primitive{Name: "int64"}
var String = Primitive{Name: "string"}

// Slice has an element type. It can be a slice of parameterized type
type Slice struct {
	Element Type
}

func (p Slice) TypeName() string        { return fmt.Sprintf("[]%s", p.Element.TypeName()) }
func (p Slice) Parameters() TypeArgList { return p.Element.Parameters() }
func (p Slice) InstantiateWithScope(scope *Scope) (Type, error) {
	t, err := p.Element.InstantiateWithScope(scope)
	return Slice{Element: t}, err
}

// Map has key and value types. It can be a map using parameterized
// types, and its parameters are the combination of the parameters of
// its key and value types
type Map struct {
	Key, Value Type
}

func (p Map) TypeName() string        { return fmt.Sprintf("map[%s]%s", p.Key.TypeName(), p.Value.TypeName()) }
func (p Map) Parameters() TypeArgList { return append(p.Key.Parameters(), p.Value.Parameters()...) }
func (p Map) InstantiateWithScope(scope *Scope) (Type, error) {
	k, err := p.Key.InstantiateWithScope(scope)
	if err != nil {
		return Map{}, err
	}
	v, err := p.Value.InstantiateWithScope(scope)
	return Map{Key: k, Value: v}, err
}

// Channel has an element type. It can be a channel of a parameterized type
type Channel struct {
	Type Type
}

func (p Channel) TypeName() string        { return fmt.Sprintf("chan %s", p.Type.TypeName()) }
func (p Channel) Parameters() TypeArgList { return p.Type.Parameters() }
func (p Channel) InstantiateWithScope(scope *Scope) (Type, error) {
	t, err := p.Type.InstantiateWithScope(scope)
	return Channel{Type: t}, err
}

// A Pointer is a pointer to a type.
type Pointer struct {
	To Type
}

func (p Pointer) TypeName() string        { return "*" + p.To.TypeName() }
func (p Pointer) Parameters() TypeArgList { return p.To.Parameters() }
func (p Pointer) InstantiateWithScope(scope *Scope) (Type, error) {
	t, err := p.To.InstantiateWithScope(scope)
	return Pointer{To: t}, err
}

// A DerivedType is a type derived from another type such as
//
//   type T int
//
// A derived type can have type parameters, and bind some or all of
// the type parameters of the underlying type.
//
//   type X(type U, type W) {...}
//
//   type T(type A) X(int,A)
//
// Then T(string) has underlying type of X(int,string)
//
type DerivedType struct {
	Name     string
	From     Type
	TypeArgs TypeArgList
}

func (p DerivedType) TypeName() string        { return p.Name }
func (p DerivedType) Parameters() TypeArgList { return p.TypeArgs }

func (p DerivedType) InstantiateWithScope(scope *Scope) (Type, error) {
	// The scope should have all the type assignments we need
	t, err := p.From.InstantiateWithScope(scope)
	if err != nil {
		return nil, err
	}
	return DerivedType{Name: MakeName(p.Name, p.TypeArgs, scope), From: t}, nil
}

// Instantiate the type with the given type list
func (p DerivedType) Instantiate(scope *Scope, types []Type) (DerivedType, error) {
	if !IsParameterized(p) {
		return p, nil
	}
	// Push the new types into the scope
	newScope := scope.New()
	err := newScope.AssignTypes(p.TypeArgs, types)
	if err != nil {
		return DerivedType{}, err
	}
	ret, err := p.InstantiateWithScope(newScope)
	if err != nil {
		return DerivedType{}, err
	}
	return ret.(DerivedType), nil
}

var Int = DerivedType{Name: "int", From: Int64}

// TypeArgReference is the reference to a type argument in a parameterized type
type TypeArgReference struct {
	Name string
}

func (p TypeArgReference) TypeName() string        { return p.Name }
func (p TypeArgReference) Parameters() TypeArgList { return nil }
func (p TypeArgReference) InstantiateWithScope(scope *Scope) (Type, error) {
	t := scope.Get(p.Name)
	if t == nil {
		return nil, fmt.Errorf("Cannot find %s", p.Name)
	}
	return t, nil
}

/*
This is a parameterized function:

func F(type T)(in T) T {
}
*/

// A Func has a name, type arguments, function arguments, and return values
type Func struct {
	Name string

	// TypeArgs are parameters to the type, and they can have constraints.
	TypeArgs TypeArgList

	Args   VarList
	Return VarList
}

// A Func is a Type
func (f Func) TypeName() string        { return f.Name }
func (f Func) Parameters() TypeArgList { return f.TypeArgs }

// Instantiate a function using the given type assignment. The
// returned func is a concrete func with no type parameters, otherwise
// instantiation fails
func (f Func) Instantiate(scope *Scope, types []Type) (Func, error) {
	// If this is already a concrete function, return
	if !IsParameterized(f) {
		return f, nil
	}

	// Push the new types into the scope
	newScope := scope.New()
	err := newScope.AssignTypes(f.TypeArgs, types)
	if err != nil {
		return Func{}, err
	}
	ret, err := f.InstantiateWithScope(newScope)
	if err != nil {
		return Func{}, err
	}
	return ret.(Func), nil
}

func (f Func) InstantiateWithScope(scope *Scope) (Type, error) {
	result := Func{Name: MakeName(f.Name, f.TypeArgs, scope)}
	var err error
	result.Args, err = f.Args.InstantiateWithScope(scope)
	if err != nil {
		return Func{}, err
	}
	result.Return, err = f.Return.InstantiateWithScope(scope)
	if err != nil {
		return Func{}, err
	}

	return result, nil
}

// A Var has an optional name, and a type.
type Var struct {
	Name string
	Type Type
}

// Instantiate a variable using type assignments in scope
func (v Var) InstantiateWithScope(scope *Scope) (Var, error) {
	result := v
	if IsParameterized(v.Type) {
		var err error
		result.Type, err = v.Type.InstantiateWithScope(scope)
		if err != nil {
			return Var{}, err
		}
	}
	return result, nil
}

// List of vars, for arg lists and return values
type VarList []Var

// Instantiate a variable list using the type assignments in scope
func (v VarList) InstantiateWithScope(scope *Scope) (VarList, error) {
	ret := VarList{}
	for _, x := range v {
		concrete, err := x.InstantiateWithScope(scope)
		if err != nil {
			return nil, err
		}
		ret = append(ret, concrete)
	}
	return ret, nil
}

/*

This is a parameterized struct:

type listNode(type T) struct {
  payload T
}
*/

type Struct struct {
	Name     string
	TypeArgs TypeArgList
	Members  VarList
	Methods  MethodList
}

type MethodList []Func

// Check if every method in m exists in t
func (m MethodList) SubsetOf(t MethodList) bool {
	for _, mth := range m {
		if destMth, ok := t.Find(mth.Name); ok {
			// Method return types and arg list must match
			if !mth.Args.Matches(destMth.Args) ||
				!mth.Return.Matches(destMth.Return) {
				return false
			}
		} else {
			return false
		}
	}
	return true
}

// Instantiate a method list using the type assignments in scope
func (v MethodList) InstantiateWithScope(scope *Scope) (MethodList, error) {
	ret := MethodList{}
	for _, x := range v {
		concrete, err := x.InstantiateWithScope(scope)
		if err != nil {
			return nil, err
		}
		ret = append(ret, concrete.(Func))
	}
	return ret, nil
}

func (s Struct) TypeName() string        { return fmt.Sprintf("%s(%v)", s.Name, s.TypeArgs) }
func (s Struct) Parameters() TypeArgList { return s.TypeArgs }

func (s Struct) InstantiateWithScope(scope *Scope) (Type, error) {
	result := Struct{Name: MakeName(s.Name, s.TypeArgs, scope)}
	var err error
	result.Members, err = s.Members.InstantiateWithScope(scope)
	if err != nil {
		return nil, err
	}
	result.Methods, err = s.Methods.InstantiateWithScope(scope)
	if err != nil {
		return nil, err
	}
	return result, nil
}

// Instantiate a struct using the given type assignment
func (s Struct) Instantiate(scope *Scope, types []Type) (Struct, error) {
	// If this is already a concrete struct, return
	if !IsParameterized(s) {
		return s, nil
	}

	// Push the new types into the scope
	newScope := scope.New()
	err := newScope.AssignTypes(s.TypeArgs, types)
	if err != nil {
		return Struct{}, err
	}
	ret, err := s.InstantiateWithScope(newScope)
	if err != nil {
		return Struct{}, err
	}
	return ret.(Struct), nil
}

/*

This is a parameterized interface

type ListNode(type T) interface {
  GetPayload() T
}
*/

type Interface struct {
	Name     string
	TypeArgs TypeArgList
	Methods  MethodList
}

func (s Interface) TypeName() string        { return fmt.Sprintf("%s(%v)", s.Name, s.TypeArgs) }
func (s Interface) Parameters() TypeArgList { return s.TypeArgs }

func (s Interface) InstantiateWithScope(scope *Scope) (Type, error) {
	result := Interface{Name: MakeName(s.Name, s.TypeArgs, scope)}
	var err error
	result.Methods, err = s.Methods.InstantiateWithScope(scope)
	if err != nil {
		return nil, err
	}
	return result, nil
}

// Instantiate an interface using the given type assignment
func (s Interface) Instantiate(scope *Scope, types []Type) (Interface, error) {
	// If this is already a concrete interface, return
	if !IsParameterized(s) {
		return s, nil
	}

	// Push the new types into the scope
	newScope := scope.New()
	err := newScope.AssignTypes(s.TypeArgs, types)
	if err != nil {
		return Interface{}, err
	}
	ret, err := s.InstantiateWithScope(newScope)
	if err != nil {
		return Interface{}, err
	}
	return ret.(Interface), nil
}

var EmptyIntf = Interface{Name: "interface{}"}

/*

A type implements an interface if it has all the functions interface
has. This only works on concrete types

*/

func (s Interface) ImplementedBy(t Type) bool {
	if IsParameterized(s) || IsParameterized(t) {
		return false
	}
	// If s has any methods, then t must be a struct
	if len(s.Methods) > 0 {
		if cStruct, ok := t.(Struct); ok {
			return s.Methods.SubsetOf(cStruct.Methods)
		}
	}
	return true
}

/*

The TypeArg is the type parameter to these constructs. We add
contracts on type parameters to specify constraints and guarantees
about them so we can compile the parameterized function, struct,
interface before the actual instantiation.

*/

type TypeArg struct {
	Name     string
	Contract Contract
}

type TypeArgList []TypeArg

// Resolve a type arg list using the type assignments in the current
// scope
func (l TypeArgList) Resolve(scope *Scope) ([]Type, error) {
	ret := make([]Type, 0, len(l))
	for _, typeArg := range l {
		// There must be a type in scope with this name
		assignedType := scope.Get(typeArg.Name)
		if assignedType == nil {
			return nil, fmt.Errorf("Cannot instantiate: %s not bound", typeArg.Name)
		}

		// Must instantiate with concrete types
		if IsParameterized(assignedType) {
			return nil, fmt.Errorf("Cannot instantiate: %s is parameterized", assignedType.TypeName())
		}

		// Type contract must be satisfied
		if !typeArg.Contract.SatisfiedBy(assignedType) {
			return nil, fmt.Errorf("Type %s does not satisfy contract for %s", assignedType.TypeName(), typeArg.Name)
		}
		ret = append(ret, assignedType)
	}
	return ret, nil
}

/*

Use a list of existing types to specify a contract. A contract can be
parameterized with types.

*/

type Contract struct {
	Name       string
	TypeParams []TypeArg
	Type       []ContractItem
}

// ContractItem is either an existing type (which can be declared
// within the contract), or a "like" clause. It implements the
// SatisfiedBy method that checks if a type satisfies the contract
type ContractItem interface {
	SatisfiedBy(*Contract, Type) bool
}

/*

A type satisfies a contract if it satisfies all contracts included in it.

*/

func (c Contract) SatisfiedBy(t Type) bool {
	for _, x := range c.Type {
		if !x.SatisfiedBy(&c, t) {
			return false
		}
	}
	return true
}

/*

A contract can include an interface. Name is optional

*/

type InterfaceContract struct {
	I Interface
}

/*

A type satisfies an interface contract if it implements the interface. Here:

 * intf must be a concrete type. Is is being used to instantiate a type
 * contract may or may not be parameterized

*/

func (c InterfaceContract) SatisfiedBy(contract *Contract, intf Type) bool {
	// intf must be a concrete type
	if IsParameterized(intf) {
		return false
	}

	return false
}

/*

A contract can include a struct

*/

type StructContract struct {
	S Struct
}

/*

A type satisfies a struct contract if it implements the interface of
the struct contract, and declares all fields in in

*/

func (c StructContract) SatisfiedBy(contract *Contract, intf Type) bool {
	return false
}

/*

A contract can include a 'like' construct:

  like(t1, t2, ...)

*/

type LikeContract struct {
	Types []TypeArg
}

/*

A type satisfies a "like" contract if it is derived from one of the types listed in the contract

*/

func (c LikeContract) SatisfiedBy(contract *Contract, intf Type) bool {
	return false
}

func assertTrue(b bool, format string, args ...interface{}) {
	if !b {
		log.Fatalf("Failed: %s", fmt.Sprintf(format, args...))
	}
}

func sanityChecks() {
	scope := NewScope()

	assertTrue(EmptyIntf.ImplementedBy(Int64), "int64 implements interface{}")

	// func f(i int) string
	f := Func{Name: "f1", Args: VarList{{Name: "i", Type: Int}}, Return: VarList{{Type: String}}}

	assertTrue(!IsParameterized(f), "f is not parameterized")
	instf, err := f.Instantiate(scope, nil)
	if err != nil {
		panic(err)
	}
	assertTrue(instf.TypeName() == f.TypeName(), "instantiated non-parameterized func is equal to itself")

	// func f(type T,type X)(a T) X
	f = Func{Name: "f2", Args: VarList{{Name: "a", Type: TypeArgReference{Name: "T"}}},
		Return:   VarList{{Type: TypeArgReference{Name: "X"}}},
		TypeArgs: TypeArgList{{Name: "T"}, {Name: "X"}}}
	instf, err = f.Instantiate(scope, []Type{Int, String})
	if err != nil {
		panic(err)
	}

	// Concrete function must be func f(i int) string
	if instf.Args[0].Type.TypeName() != Int.TypeName() {
		panic("Wrong arg type")
	}
	if instf.Return[0].Type.TypeName() != String.TypeName() {
		panic("Wrong return type")
	}
}

func main() {

	sanityChecks()
}
