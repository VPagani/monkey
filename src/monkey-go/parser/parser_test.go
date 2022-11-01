package parser

import (
	"fmt"
	"monkey/ast"
	"monkey/lexer"
	"testing"
)

func testParse(t *testing.T, input string) *ast.Program {
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p)

	return program
}

func testParseProgram(t *testing.T, expectedStatementsCount int, input string) *ast.Program {
	program := testParse(t, input)

	statementCount := len(program.Statements)
	if statementCount != expectedStatementsCount {
		t.Fatalf("program.Statements does not contain %d statements. got=%d", expectedStatementsCount, statementCount)
	}

	return program
}

func testParseExpression(t *testing.T, input string) ast.Expression {
	program := testParseProgram(t, 1, input)

	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", stmt.Expression)
	}

	return stmt.Expression
}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()

	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))

	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}

	t.FailNow()
}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf("s.TokenLiteral not 'let'. got=%T", s)
		return false
	}

	letStmt, ok := s.(*ast.LetStatement)
	if !ok {
		t.Errorf("s not *ast.LetStatement. got=%T", s)
		return false
	}

	if letStmt.Name.Value != name {
		t.Errorf("letStmt.Name.Value not '%s'. got=%s", name, letStmt.Name.TokenLiteral())
		return false
	}

	return true
}

func TestLetStatements(t *testing.T) {
	tests := []struct {
		input              string
		expectedIdentifier string
		expectedValue      interface{}
	}{
		{"let x = 5;", "x", 5},
		{"let y = true;", "y", true},
		{"let foobar = y;", "foobar", "y"},
	}

	for _, tt := range tests {
		program := testParseProgram(t, 1, tt.input)

		stmt := program.Statements[0]
		if !testLetStatement(t, stmt, tt.expectedIdentifier) {
			return
		}

		val := stmt.(*ast.LetStatement).Value
		if !testLiteralExpression(t, val, tt.expectedValue) {
			return
		}
	}
}

func TestReturnStatements(t *testing.T) {
	tests := []struct {
		input         string
		expectedValue interface{}
	}{
		{"return 5;", 5},
		{"return true;", true},
		{"return foobar;", "foobar"},
	}

	for _, tt := range tests {
		program := testParseProgram(t, 1, tt.input)

		stmt := program.Statements[0]

		returnStmt, ok := stmt.(*ast.ReturnStatement)
		if !ok {
			t.Fatalf("stmt not *ast.ReturnStatement. got=%T", stmt)
			return
		}

		if returnStmt.TokenLiteral() != "return" {
			t.Fatalf("returnStmt.TokenLiteral not 'return', got %q", returnStmt.TokenLiteral())
			return
		}

		if !testLiteralExpression(t, returnStmt.ReturnValue, tt.expectedValue) {
			return
		}
	}
}

func testIdentifier(t *testing.T, exp ast.Expression, value string) bool {
	identifier, ok := exp.(*ast.Identifier)
	if !ok {
		t.Fatalf("exp not *ast.Identifier. got=%T", exp)
		return false
	}

	if identifier.Value != value {
		t.Errorf("identifier.Value not %s. got=%s", value, identifier.Value)
		return false
	}

	if identifier.TokenLiteral() != value {
		t.Errorf("identifier.TokenLiteral not %s. got=%s", value, identifier.TokenLiteral())
		return false
	}

	return true
}

func testBooleanLiteral(t *testing.T, exp ast.Expression, value bool) bool {
	boolean, ok := exp.(*ast.BooleanLiteral)
	if !ok {
		t.Errorf("expression is not *ast.BooleanLiteral. got=%T", exp)
		return false
	}

	if boolean.Value != value {
		t.Errorf("boolean.Value not %t. got=%t", value, boolean.Value)
		return false
	}

	if boolean.TokenLiteral() != fmt.Sprintf("%t", value) {
		t.Errorf("boolean.TokenLiteral not %t. got=%s", value, boolean.TokenLiteral())
		return false
	}

	return true
}

func testIntegerLiteral(t *testing.T, exp ast.Expression, value int64) bool {
	integer, ok := exp.(*ast.IntegerLiteral)
	if !ok {
		t.Errorf("expression is not *ast.IntegerLiteral. got=%T", exp)
		return false
	}

	if integer.Value != value {
		t.Errorf("integer.Value not %d. got=%d", value, integer.Value)
		return false
	}

	if integer.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("integer.TokenLiteral not %d. got=%s", value, integer.TokenLiteral())
		return false
	}

	return true
}

func testStringLiteral(t *testing.T, exp ast.Expression, value string) bool {
	literal, ok := exp.(*ast.StringLiteral)
	if !ok {
		t.Fatalf("exp not *ast.StringLiteral. got=%T", literal)
		return false
	}

	if literal.Value != value {
		t.Errorf("literal.Value not %q. got=%q", value, literal.Value)
		return false
	}

	return true
}

func testLiteralExpression(t *testing.T, exp ast.Expression, expected interface{}) bool {
	switch v := expected.(type) {
	case bool:
		return testBooleanLiteral(t, exp, v)
	case int:
		return testIntegerLiteral(t, exp, int64(v))
	case int64:
		return testIntegerLiteral(t, exp, v)
	case string:
		return testIdentifier(t, exp, v)
	}

	t.Errorf("type of expression not handled. got=%T", exp)

	return false
}

func testInfixExpression(t *testing.T, exp ast.Expression, left interface{}, operator string, right interface{}) bool {
	opExp, ok := exp.(*ast.InfixExpression)
	if !ok {
		t.Errorf("expression is not ast.InfixExpression. got=%T(%s)", exp, exp)
		return false
	}

	if !testLiteralExpression(t, opExp.Left, left) {
		return false
	}

	if opExp.Operator != operator {
		t.Errorf("exp.Operator is not '%s'. got=%q", operator, opExp.Operator)
		return false
	}

	if !testLiteralExpression(t, opExp.Right, right) {
		return false
	}

	return true
}

func TestIdentifier(t *testing.T) {
	input := "foobar;"

	expression := testParseExpression(t, input)

	testIdentifier(t, expression, "foobar")
}

func TestBooleanLiteralExpression(t *testing.T) {
	tests := []struct {
		input string
		value bool
	}{
		{"true;", true},
		{"false;", false},
	}

	for _, tt := range tests {
		expression := testParseExpression(t, tt.input)

		testBooleanLiteral(t, expression, tt.value)
	}
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"

	expression := testParseExpression(t, input)

	testLiteralExpression(t, expression, 5)
}

func TestStringLiteralExpression(t *testing.T) {
	input := `"hello world"`

	expression := testParseExpression(t, input)

	testStringLiteral(t, expression, "hello world")
}

func TestParsingPrefixExpressions(t *testing.T) {
	prefixtests := []struct {
		input        string
		operator     string
		integerValue int64
	}{
		{"!5;", "!", 5},
		{"-15;", "-", 15},
	}

	for _, tt := range prefixtests {
		expression := testParseExpression(t, tt.input)

		prefixExp, ok := expression.(*ast.PrefixExpression)
		if !ok {
			t.Fatalf("stmt is not ast.PrefixExpression. got=%T", expression)
		}

		if prefixExp.Operator != tt.operator {
			t.Fatalf("prefixExp.Operator is not '%s'. got=%s", tt.operator, prefixExp.Operator)
		}

		if !testIntegerLiteral(t, prefixExp.Right, tt.integerValue) {
			return
		}
	}
}

func TestParsingInfixExpressions(t *testing.T) {
	infixTests := []struct {
		input      string
		leftValue  interface{}
		operator   string
		rightValue interface{}
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
		{"true == true", true, "==", true},
		{"true != false", true, "!=", false},
		{"false == false", false, "==", false},
	}

	for _, tt := range infixTests {
		expression := testParseExpression(t, tt.input)

		testInfixExpression(t, expression, tt.leftValue, tt.operator, tt.rightValue)
	}
}

func TestOperatorPrecedenceParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			"-a + b",
			"((-a) + b)",
		},
		{
			"!-a",
			"(!(-a))",
		},
		{
			"a + b + c",
			"((a + b) + c)",
		},
		{
			"a + b - c",
			"((a + b) - c)",
		},
		{
			"a * b * c",
			"((a * b) * c)",
		},
		{
			"a * b / c",
			"((a * b) / c)",
		},
		{
			"a + b / c",
			"(a + (b / c))",
		},
		{
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		},
		{
			"3 + 4; -5 * 5",
			"(3 + 4)((-5) * 5)",
		},
		{
			"5 > 4 == 3 < 4",
			"((5 > 4) == (3 < 4))",
		},
		{
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		},
		{
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
		{
			"true",
			"true",
		},
		{
			"false",
			"false",
		},
		{
			"3 > 5 == false",
			"((3 > 5) == false)",
		},
		{
			"3 < 5 == true",
			"((3 < 5) == true)",
		},
		{
			"1 + (2 + 3) + 4",
			"((1 + (2 + 3)) + 4)",
		},
		{
			"(5 + 5) * 2",
			"((5 + 5) * 2)",
		},
		{
			"2 / (5 + 5)",
			"(2 / (5 + 5))",
		},
		{
			"-(5 + 5)",
			"(-(5 + 5))",
		},
		{
			"!(true == true)",
			"(!(true == true))",
		},
		{
			"a + add(b * c) + d",
			"((a + add((b * c))) + d)",
		},
		{
			"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
			"add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
		},
		{
			"add(a + b + c * d / f + g)",
			"add((((a + b) + ((c * d) / f)) + g))",
		},
	}

	for _, tt := range tests {
		program := testParse(t, tt.input)

		actual := program.String()
		if actual != tt.expected {
			t.Errorf("expected=%q, got=%q", tt.expected, actual)
		}
	}
}

func TestIfExpression(t *testing.T) {
	input := "if (x < y) { x }"

	expression := testParseExpression(t, input)

	ifExp, ok := expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf("expression is not ast.IfExpression. got%T", expression)
	}

	if !testInfixExpression(t, ifExp.Condition, "x", "<", "y") {
		return
	}

	stmtCount := len(ifExp.Consequence.Statements)
	if stmtCount != 1 {
		t.Errorf("consequence is not 1 statments. got=%d\n", stmtCount)
	}

	consequece, ok := ifExp.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Statements[0] is not ast.ExpressionStatement. got=%T", ifExp.Consequence.Statements[0])
	}

	if !testIdentifier(t, consequece.Expression, "x") {
		return
	}

	if ifExp.Alternative != nil {
		t.Errorf("ifExp.Alternative was not nil. got=%+v", ifExp.Alternative)
	}
}

func TestIfElseExpression(t *testing.T) {
	input := "if (x == y) { x } else { y }"

	expression := testParseExpression(t, input)

	ifExp, ok := expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf("expression is not ast.IfExpression. got%T", expression)
	}

	if !testInfixExpression(t, ifExp.Condition, "x", "==", "y") {
		return
	}

	stmtCount := len(ifExp.Consequence.Statements)
	if stmtCount != 1 {
		t.Errorf("consequence is not 1 statments. got=%d\n", stmtCount)
	}

	consequece, ok := ifExp.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Statements[0] is not ast.ExpressionStatement. got=%T", ifExp.Consequence.Statements[0])
	}

	if !testIdentifier(t, consequece.Expression, "x") {
		return
	}

	alternative, ok := ifExp.Alternative.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Statements[0] is not ast.ExpressionStatement. got=%T", ifExp.Alternative.Statements[0])
	}

	if !testIdentifier(t, alternative.Expression, "y") {
		return
	}
}

func TestFunctionLiteralParsing(t *testing.T) {
	input := "fn(x, y) { x + y; }"

	expression := testParseExpression(t, input)

	function, ok := expression.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf("expression isnot ast.FunctionLiteral. got=%T", expression)
	}

	functionParams := len(function.Parameters)
	if functionParams != 2 {
		t.Fatalf("function literal parameters wrong. want 2, got=%d\n", functionParams)
	}

	testLiteralExpression(t, function.Parameters[0], "x")
	testLiteralExpression(t, function.Parameters[1], "y")

	functionStmts := len(function.Body.Statements)
	if functionStmts != 1 {
		t.Fatalf("function.Body.Statements has not 1 statements. got=%d\n", functionStmts)
	}

	bodyStmt, ok := function.Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("function body statement is not ast.ExpressionStatement. got=%T", bodyStmt)
	}

	testInfixExpression(t, bodyStmt.Expression, "x", "+", "y")
}

func TestFunctionParameterParsing(t *testing.T) {
	tests := []struct {
		input          string
		expectedParams []string
	}{
		{input: "fn() {};", expectedParams: []string{}},
		{input: "fn(x) {};", expectedParams: []string{"x"}},
		{input: "fn(x, y, z) {};", expectedParams: []string{"x", "y", "z"}},
	}

	for _, tt := range tests {
		expression := testParseExpression(t, tt.input)

		function := expression.(*ast.FunctionLiteral)

		if len(function.Parameters) != len(tt.expectedParams) {
			t.Errorf("length parameters wrong. want %d, got=%d\n", len(tt.expectedParams), len(function.Parameters))
		}

		for i, ident := range tt.expectedParams {
			testLiteralExpression(t, function.Parameters[i], ident)
		}
	}
}

func TestCallExpressionParsing(t *testing.T) {
	input := "add(1, 2* 3, 4 +5);"

	expression := testParseExpression(t, input)

	callExp, ok := expression.(*ast.CallExpression)
	if !ok {
		t.Fatalf("expression is not ast.CallExpression. got=%T", expression)
	}

	if !testIdentifier(t, callExp.Function, "add") {
		return
	}

	argCount := len(callExp.Arguments)
	if argCount != 3 {
		t.Fatalf("wrong length of arguments. got=%d", argCount)
	}

	testLiteralExpression(t, callExp.Arguments[0], 1)
	testInfixExpression(t, callExp.Arguments[1], 2, "*", 3)
	testInfixExpression(t, callExp.Arguments[2], 4, "+", 5)
}
