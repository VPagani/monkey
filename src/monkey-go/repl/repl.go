package repl

import (
	"bufio"
	"fmt"
	"io"
	"monkey/lexer"
	"monkey/parser"
	"monkey/token"
)

const PROMPT = ">> "

type ReplType int

const (
	RLPL ReplType = iota
	RPPL
	REPL
)

const REPL_TYPE = RPPL

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Fprint(out, PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)

		switch REPL_TYPE {
		case RLPL:
			printProgramTokens(out, l)
		case RPPL:
			printProgramParsed(out, l)
		}
	}
}

func printProgramTokens(out io.Writer, l *lexer.Lexer) {
	for tok := l.NextToken(); tok.Type != token.EOF; tok = l.NextToken() {
		fmt.Fprintf(out, "%+v\n", tok)
	}
}

func printProgramParsed(out io.Writer, lexer *lexer.Lexer) bool {
	p := parser.New(lexer)

	program := p.ParseProgram()
	if len(p.Errors()) != 0 {
		printParseErrors(out, p.Errors())
		return false
	}

	io.WriteString(out, program.String())
	io.WriteString(out, "\n")
	return true
}

const MONKEY_FACE = `              __,__
     .--.  .-"     "-.  .--.
    / .. \/  .-. .-.  \/ .. \
   | |  '|  /   Y   \  |'  | |
   | \   \  \ 0 | 0 /  /   / |
    \ '- ,\.-"""""""-./, -' /
     ''-' /_   ^ ^   _\ '-''
         |  \._   _./  |
         \   \ '~' /   /
          '._ '-=-' _.'
             '-----'
`

func printParseErrors(out io.Writer, errors []string) {
	io.WriteString(out, MONKEY_FACE)
	io.WriteString(out, "Woops! We ran into some monkey business here!\n")
	io.WriteString(out, " parser errors:\n")

	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
