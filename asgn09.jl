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

function lookup(id, env)
  for pair in env
    if pair.first == id
      return pair.second
    end
  end
  error("Id not found in env")
end


function interp(expr::ExprC, env::Array)::Value
  @match expr begin
    app::AppC => 
    let func = interp(app.func, env)
      return func.op(interp(app.args[1], env), interp(app.args[2], env))
    end
    num::NumC => return NumV(num.n)
    str::StringC => return StringV(str.s)
    id::IdC => return lookup(id.symbol, env)

    
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
