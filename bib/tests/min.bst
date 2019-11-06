ENTRY { field }{}{}

FUNCTION {test}
{ cite$ write$ newline$ }

FUNCTION {print}
{ "this is a test" warning$ }

FUNCTION {article}
{"de la Cierva y Codorn{\’\i}u, Juan"
#1 "{ff - }{vv - }{ll}" format.name$ top$}

READ

ITERATE {test}

