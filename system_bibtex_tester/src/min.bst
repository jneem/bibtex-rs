ENTRY { field title author }{}{}

FUNCTION {write_field}
{
    duplicate$
    empty$
    { pop$ pop$ }
    { swap$ write$ write$ newline$ }
    if$
}

FUNCTION {write_entry}
{
    cite$ write$ newline$
    "field: " field write_field
    "title: " title write_field
    "author: " author write_field
}

FUNCTION {article}
{}

READ

ITERATE {write_entry}

