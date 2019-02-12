

b = g(a)
x = g(y, z)
z = h(y)
y = f(a)


F(F(b,x),F(F(h(y),g(y,z)),z))
F(F(g(a),g(y,z)),F(F(z,x),h(f(a))))

val t1 = Fun ([#"F"], [Fun ([#"F"], [Var [#"b"], Var [#"x"]]),
    Fun ([#"F"], [Fun ([#"F"], [Fun ([#"h"], [Var [#"y"]]),
        Fun ([#"g"], [Var [#"y"], Var [#"z"]])]), Var [#"z"]])])

val t2 = Fun ([#"F"], [Fun ([#"F"], [Fun ([#"g"], [Var [#"a"]]), Fun ([#"g"], [Var [#"y"], Var [#"z"]])]), Fun ([#"F"], [Fun ([#"F"], [Var [#"z"], Var [#"x"]]), Fun ([#"h"], [Fun ([#"f"], [Var [#"a"]])])])])
