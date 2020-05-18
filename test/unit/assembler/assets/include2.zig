//! test for including a c file

const foo = @cImport{
    @cInclude("foo.h");
    @cInclude("bar.h");
};
