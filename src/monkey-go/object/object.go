package object

import (
	"bytes"
	"fmt"
	"monkey/ast"
	"strings"
)

type ObjectType string

const (
	NULL_OBJ         ObjectType = "NULL"
	INTEGER_OBJ      ObjectType = "INTEGER"
	BOOLEAN_OBJ      ObjectType = "BOOLEAN"
	STRING_OBJ       ObjectType = "STRING"
	ARRAY_OBJ        ObjectType = "ARRAY"
	FUNCTION_OBJ     ObjectType = "FUNCTION"
	RETURN_VALUE_OBJ ObjectType = "RETURN_VALUE"
	ERROR_OBJ        ObjectType = "ERROR"
	BUILTIN_OBJ      ObjectType = "BUILTIN"
)

type Object interface {
	Type() ObjectType
	Inspect() string
}

type Null struct{}

func (obj *Null) Type() ObjectType { return NULL_OBJ }
func (obj *Null) Inspect() string  { return "null" }

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

type String struct {
	Value string
}

func (s *String) Type() ObjectType { return STRING_OBJ }
func (s *String) Inspect() string  { return s.Value }

type Array struct {
	Elements []Object
}

func (a *Array) Type() ObjectType { return ARRAY_OBJ }
func (a *Array) Inspect() string {
	var out bytes.Buffer

	elements := []string{}
	for _, e := range a.Elements {
		elements = append(elements, e.Inspect())
	}

	out.WriteString("[")
	out.WriteString(strings.Join(elements, ", "))
	out.WriteString("]")

	return out.String()
}

type ReturnValue struct {
	Value Object
}

func (obj *ReturnValue) Type() ObjectType { return RETURN_VALUE_OBJ }
func (obj *ReturnValue) Inspect() string  { return obj.Value.Inspect() }

type Function struct {
	Parameters []*ast.Identifier
	Body       *ast.BlockStatement
	Env        *Environment
}

func (f *Function) Type() ObjectType { return FUNCTION_OBJ }
func (f *Function) Inspect() string {
	var out bytes.Buffer

	params := []string{}

	for _, p := range f.Parameters {
		params = append(params, p.String())
	}

	out.WriteString("fn")
	out.WriteString("(")
	out.WriteString(strings.Join(params, ", "))
	out.WriteString(") {\n")
	out.WriteString(f.Body.String())
	out.WriteString("\n}")

	return out.String()
}

type Builtin struct {
	Fn BuiltinFunction
}

type BuiltinFunction func(args ...Object) Object

func (b *Builtin) Type() ObjectType { return BUILTIN_OBJ }
func (b *Builtin) Inspect() string  { return "builtin function" }

type Error struct {
	Message string
}

func (obj *Error) Type() ObjectType { return ERROR_OBJ }
func (obj *Error) Inspect() string  { return "ERROR: " + obj.Message }
