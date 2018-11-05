package main

import (
	"fmt"
)

// Find a method by its name, return func{}, true if found
func (m MethodList) Find(name string) (Func, bool) {
	for _, f := range m {
		if f.Name == name {
			return f, true
		}
	}
	return Func{}, false
}

// Find a var by its name, return var{}, true if found
func (v VarList) Find(name string) (Var, bool) {
	for _, f := range v {
		if f.Name == name {
			return f, true
		}
	}
	return Var{}, false
}

// Matches returns true if two varlists have the same type variables
// in the same order
func (v VarList) Matches(x VarList) bool {
	if len(v) == len(x) {
		for i := 0; i < len(v); i++ {
			if v[i].Type != x[i].Type {
				return false
			}
		}
		return true
	}
	return false
}

// Scope is for type resolution
type Scope struct {
	Parent  *Scope
	Current map[string]Type
}

func NewScope() *Scope {
	return &Scope{Current: make(map[string]Type)}
}

func (s *Scope) New() *Scope {
	return &Scope{Parent: s, Current: make(map[string]Type)}
}

// Assign name=t in the scope
func (s *Scope) Assign(name string, t Type) {
	if _, ok := s.Current[name]; ok {
		// Don't do this
		panic(name)
	}
	s.Current[name] = t
}

// Assign args[i].Name=types[i] in the scope
func (s *Scope) AssignTypes(args TypeArgList, types []Type) error {
	if len(args) != len(types) {
		return fmt.Errorf("Type arg list size mismatch")
	}
	for i, x := range args {
		s.Assign(x.Name, types[i])
	}
	return nil
}

func (s *Scope) Get(name string) Type {
	if t, ok := s.Current[name]; ok {
		return t
	}
	if s.Parent != nil {
		return s.Parent.Get(name)
	}
	return nil
}

func MakeName(name string, typeArgs TypeArgList, scope *Scope) string {
	for _, x := range typeArgs {
		assignedType := scope.Get(x.Name)
		name = fmt.Sprintf("%s_%s=%s", name, x.Name, assignedType.TypeName())
	}
	return name
}
