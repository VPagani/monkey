package object

import "fmt"

type ObjectType string

const (
	INTEGER_OBJ      ObjectType = "INTEGER"
	BOOLEAN_OBJ      ObjectType = "BOOLEAN"
	NULL_OBJ         ObjectType = "NULL"
	RETURN_VALUE_OBJ ObjectType = "RETURN_VALUE"
	ERROR_OBJ        ObjectType = "ERROR"
)

type Object interface {
	Type() ObjectType
	Inspect() string
}

type Integer struct {
	Value int64
}

func (obj *Integer) Type() ObjectType { return INTEGER_OBJ }
func (obj *Integer) Inspect() string  { return fmt.Sprintf("%d", obj.Value) }

type Boolean struct {
	Value bool
}

func (obj *Boolean) Type() ObjectType { return BOOLEAN_OBJ }
func (obj *Boolean) Inspect() string  { return fmt.Sprintf("%t", obj.Value) }

type Null struct{}

func (obj *Null) Type() ObjectType { return NULL_OBJ }
func (obj *Null) Inspect() string  { return "null" }

type ReturnValue struct {
	Value Object
}

func (obj *ReturnValue) Type() ObjectType { return RETURN_VALUE_OBJ }
func (obj *ReturnValue) Inspect() string  { return obj.Value.Inspect() }

type Error struct {
	Message string
}

func (obj *Error) Type() ObjectType { return RETURN_VALUE_OBJ }
func (obj *Error) Inspect() string  { return "ERROR: " + obj.Message }

type Environment struct {
	store map[string]Object
}

func NewEnvironment() *Environment {
	s := make(map[string]Object)
	return &Environment{store: s}
}

func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name]
	return obj, ok
}

func (e *Environment) Set(name string, value Object) Object {
	e.store[name] = value
	return value
}
