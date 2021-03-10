#import Pkg
#Pkg.add("Match")
using Test
using Match

abstract type ExprC end

struct NumC <: ExprC n::Int end
struct StringC <: ExprC s end
struct IdC <: ExprC symbol end
struct IfC <: ExprC 
  cond
  t_case
  f_case 
end
struct LamC <: ExprC 
  params
  body
end
struct AppC <: ExprC 
  func
  args
end

abstract type Value end

struct NumV <: Value n::Int end
struct BooleanV <: Value b::Bool end
struct StringV <: Value s::String end
struct PrimV <: Value op end
struct ClosV <: Value
  params
  body::ExprC
  env
end

function addition(n1::Value, n2::Value)::Value
  if typeof(n1) == NumV && typeof(n2) == NumV
    n1, n2 = n1.n, n2.n
    return NumV(n1 + n2)
  end
end

function subtraction(n1::Value, n2::Value)::Value
  if typeof(n1) == NumV && typeof(n2) == NumV
    n1, n2 = n1.n, n2.n
    return NumV(n1 - n2)
  end
end

function division(n1::Value, n2::Value)::Value
  if typeof(n1) == NumV && typeof(n2) == NumV
    n1, n2 = n1.n, n2.n
    if n2 == 0
      error("Division denominator cannot be zero")
    end
    return NumV(n1 / n2)
  end
end

function multiplication(n1::Value, n2::Value)::Value
  if typeof(n1) == NumV && typeof(n2) == NumV
    n1, n2 = n1.n, n2.n
    return NumV(n1 * n2)
  end
end

function leq(n1::Value, n2::Value)::Value
  if typeof(n1) == NumV && typeof(n2) == NumV
    n1, n2 = n1.n, n2.n
    return BooleanV(n1 <= n2)
  end
end

function lookup(id, env)
  for pair in env
    if pair.first == id
      return pair.second
    end
  end
  error("Id not found in env")
end

function build_new_env(args, params, env)
  if length(args) == 0 && length(params) == 0
    return env
  elseif length(args) == 0 || length(params) == 0
    error("Number of args and params don't match")
  else
    return build_new_env(args[2:end], params[2:end], push!(env, Pair(first(params), first(args))))
  end
end

function interp(expr::ExprC, env::Array)::Value
  @match expr begin
    num::NumC => return NumV(num.n)
    str::StringC => return StringV(str.s)
    id::IdC => return lookup(id.symbol, env)
    lam::LamC =>
      return ClosV(lam.params, lam.body, env)
    if_expr::IfC => 
      let cond = interp(if_expr.cond, env)
        if typeof(cond) == BooleanV
          if cond.b
            interp(if_expr.t_case, env)
          else
            interp(if_expr.f_case, env)
          end
        else
          error("If condition not boolean")
        end
      end
    app::AppC => 
      let func = interp(app.func, env)
        # had issues doing a nested match so this is a workaround
        if typeof(func) == PrimV
          return func.op(interp(app.args[1], env), interp(app.args[2], env))
        elseif typeof(func) == ClosV
          argvals = map(arg -> interp(arg, env) , app.args)
          new_env = build_new_env(argvals, func.params, env)
          interp(func.body, new_env)
        else
          error("Cannot apply")
        end
      end
    _ => error("Interp error: invalid expression")
  end
end


top_env = [
  Pair('t', BooleanV(true)),
  Pair('f', BooleanV(false)),
  Pair('+', PrimV(addition)),
  Pair('-', PrimV(subtraction)),
  Pair('*', PrimV(multiplication)),
  Pair('/', PrimV(division)),
  Pair("<=", PrimV(leq))
]


# TEST CASES
@test interp(AppC(IdC('+'), [NumC(2), NumC(2)]), top_env) == NumV(4)
@test interp(AppC(IdC('*'), [NumC(2), NumC(2)]), top_env) == NumV(4)
@test interp(AppC(IdC('/'), [NumC(2), NumC(2)]), top_env) == NumV(1)
@test interp(AppC(IdC('-'), [NumC(2), NumC(2)]), top_env) == NumV(0)
@test interp(AppC(IdC('-'), [(AppC(IdC('+'), [NumC(2), NumC(2)])), 
                              NumC(2)]), top_env) == NumV(2)
@test interp(AppC(IdC('-'), [(AppC(IdC('+'), [NumC(2), NumC(2)])),
                             (AppC(IdC('+'), [NumC(2), NumC(2)]))]), top_env) == NumV(0)
@test interp(StringC("Hello!"), top_env) == StringV("Hello!")
@test interp(IdC('t'), top_env) == BooleanV(true)
@test interp(IdC('f'), top_env) == BooleanV(false)
@test interp(AppC(IdC("<="), [NumC(2), NumC(2)]), top_env) == BooleanV(true)
@test interp(AppC(IdC("<="), [NumC(1), NumC(2)]), top_env) == BooleanV(true)
@test interp(AppC(IdC("<="), [NumC(2), NumC(1)]), top_env) == BooleanV(false)


@test interp(IfC(AppC(IdC("<="), [NumC(1), NumC(2)]), 
                 NumC(1), 
                 NumC(2)), top_env) == NumV(1)
@test interp(IfC(AppC(IdC("<="), [NumC(2), NumC(1)]), 
                 NumC(1), 
                 NumC(2)), top_env) == NumV(2)


@test interp(LamC(['x'], AppC(IdC("<="), [NumC(2), NumC(1)])), top_env).params == ClosV(['x'], AppC(IdC("<="), [NumC(2), NumC(1)]), top_env).params
@test interp(LamC(['x'], AppC(IdC("<="), [NumC(2), NumC(1)])), top_env).env == ClosV(['x'], AppC(IdC("<="), [NumC(2), NumC(1)]), top_env).env


@test interp(AppC(LamC(['x'], AppC(IdC('+'), [IdC('x'), NumC(2)])), [NumC(3)]), top_env) == NumV(5)
@test interp(AppC(LamC(['x', 'y'], AppC(IdC('+'), [IdC('x'), IdC('y')])), [NumC(3), NumC(2)]), top_env) == NumV(5)

@test interp(AppC(LamC(['x', 'y'], AppC(IdC('-'), [IdC('x'), IdC('y')])), [NumC(3), NumC(2)]), top_env) == NumV(1)
@test interp(AppC(LamC(['x', 'y'], AppC(IdC('-'), [IdC('y'), IdC('x')])), [NumC(3), NumC(2)]), top_env) == NumV(-1)
@test interp(AppC(LamC(["subtrctr", 'x', 'y'], AppC(IdC("subtrctr"), [IdC('y'), IdC('x')])), [LamC(['x', 'y'], AppC(IdC('-'), [IdC('y'), IdC('x')])), NumC(3), NumC(2)]), top_env) == NumV(-1)

# At this point ~4 hours in interp works with pregenerated AST
# now I want to look into parsing 

#= Trying out built in parsing
ex1 = Meta.parse("2 + 2")
println(typeof(ex1))
println(ex1.head)
println(ex1.args)

ex2 = Meta.parse("x -> x + 2")
println(typeof(ex2))
println(ex2.head)
println(ex2.args)

ex3 = Meta.parse("((f, x) -> f(x))(x -> x + x, 2)")
println(typeof(ex3))
println(ex3.head)
println(ex3.args)

ex4 = Meta.parse("(x, y) -> x + y")
println(typeof(ex4))
println(ex4.head)
println(ex4.args)
=#

function parse_wtuq(parsed_expr)
  if typeof(parsed_expr) == Symbol
    return IdC(String(parsed_expr))
  elseif typeof(parsed_expr) == Int
    return NumC(parsed_expr)
  elseif typeof(parsed_expr) == String
    return StringC(parsed_expr)
  elseif parsed_expr.head == :->
    println("lambda")
  elseif length(parsed_expr.args) == 0
    error("No arguments, this is bad")
  elseif parsed_expr.args[1] in [:+, :-, :*, :/, :<=]
    return AppC(parse_wtuq(parsed_expr.args[1]), [parse_wtuq(parsed_expr.args[2]), parse_wtuq(parsed_expr.args[3])])
  end
end

parse_wtuq(Meta.parse("x -> x + x"))
@test parse_wtuq(Meta.parse("x + 2")).args == [IdC("x"), NumC(2)]
@test parse_wtuq(Meta.parse("x + 2")).func == IdC("+")
@test parse_wtuq(Meta.parse("5")) == NumC(5)
@test parse_wtuq(Meta.parse("x")) == IdC("x")
@test parse_wtuq(Meta.parse("\"x\"")) == StringC("x")