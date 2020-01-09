ENTRY { field title author }{}{}

FUNCTION {write_key}
{ cite$ write$ newline$ }

FUNCTION {article}
{}

READ

ITERATE {write_key}

