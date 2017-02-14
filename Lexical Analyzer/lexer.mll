{
(* Lexer file for OCTA programming language *)
 exception Eof


}

let letter = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let identifier = letter (letter|digit|'_')*


rule token = parse
	[' ' '\t' '\n']    			{ token lexbuf }     (* skip blanks *)
	| '+'            		{ PLUS }
	| '-'            		{ MINUS }
	| '*'            		{ TIMES }
	| '/'            		{ DIVISION }
	| '('            		{ LPAREN }
	| ')'            		{ RPAREN }
	| "#"					{line_comment lexbuf}				(*Comments*)
	| '{'					{LBRACE}
	| '}'					{RBRACE}
	| ';'					{SEMICOLON}
	| ','					{COMMA}
	| '='					{ASSIGN}
	| "!="					{NOTEQUAL}
	| '<'					{LT}
	| "<="					{LEQ}
	| '>' 					{GT}
	| ">="					{GEQ}
	| "=="					{COMPARISON}
	| "AND" 				{AND}
	| "OR"					{OR}
	| "if"					{IF}
	| "else"				{ELSE}
	| "elif"				{ELIF}
	| "for"					{FOR}
	| "return"				{RETURN}
	| "def"					{DEF}
	| "int"					{INT}
	| "float"				{FLOAT}
	| "string"				{STRING}
	| "boolean"				{BOOLEAN}
	| "True"				{TRUE}
	| "False"				{FALSE}
	| "while"				{WHILE}
	| "balance"				{BALANCE}
	| "loan"				{LOAN}
	| "interest_rate"		{INTEREST_RATE}
	| "months"				{MONTHS}
	| "payment"				{PAYMENT}
	| "print"				{PRINT}
	| (digit)+ '.' (digit)+ as lxm 	{FLOAT_LITERAL(float_of_string lxm)}
	| digit+ as lxm			{INT_LITERAL(int_of_string lxm)}
	| identifier as lxm 	{ID(lxm)}
	| '"' ( ('\\'_ | [^'"'])* as lxm) '"'  		{STRING_LITERAL(lxm)}
	| _ as char				{raise (Failure("Illegal character"))}
	| eof 					{raise Eof}



and line_comment = parse
	"#"					{ token lexbuf }
	| _						{ line_comment lexbuf }





      