ENTRY { field title author }{}{}

FUNCTION {write_key}
{ cite$ write$ newline$ }

FUNCTION {print}
{ "this is a test" warning$ }

FUNCTION {article}
{}

READ

ITERATE {write_key}

