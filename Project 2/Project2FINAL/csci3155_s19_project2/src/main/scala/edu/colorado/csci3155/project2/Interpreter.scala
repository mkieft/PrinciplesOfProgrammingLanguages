package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)
        case Line(l) => {
            val v = evalExpr(l, env)
            v match{
                case NumValue(x) => FigValue(new MyCanvas(List(Polygon(List((0,0),(0,x))))))
                case _ => throw new IllegalArgumentException("Length must be a number")
            }

        } //TODO: Handle a line object

        case EquiTriangle(sideLength) => {
            val v = evalExpr(sideLength,env)
            v match{
                case NumValue(x) => FigValue(new MyCanvas(List(Polygon(List((0,0),(x,0),(x/2, (math.sqrt(x)*x)/2))))))
                case _=> throw new IllegalArgumentException("Side length must be a number")
            }
        } // TODO: Handle Equilateral Triangle

        case Rectangle(sideLength) => {
            val v = evalExpr(sideLength,env)
            v match{
                case NumValue(x) => FigValue(new MyCanvas(List(Polygon(List((0,0),(0,x),(x,x),(x,0))))))
                case _ => throw new IllegalArgumentException("Side length must be a number")
            }

        } // TODO: Handle square given the side length
        case Circle(rad) => {
            val v = evalExpr(rad,env)
            v match{
                case NumValue(x) => new FigValue(new MyCanvas(List(MyCircle((x,x),x))))
                case _=> throw new IllegalArgumentException("Radius must be a number")
            }
//            FigValue(new MyCanvas(MyCircle{(v,v),v)}))

        }//TODO: Handle circle
        case Plus (e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1,v2) match{
                case (FigValue(x1), FigValue(x2))=> FigValue(x1.overlap(x2))
                case (NumValue(x1), NumValue(x2))=> NumValue(x1+x2)
                case _ => throw new IllegalArgumentException("Cannot add figure with number")

            }

            //(ValueOps.plus)
        }// TODO: Handle addition of numbers or figures
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1,v2) match{
                case (NumValue(x1), NumValue(x2))=> NumValue(x1*x2)
                case (FigValue(x1), FigValue(x2))=> FigValue(x1.placeRight(x2))
                case _ => throw new IllegalArgumentException("Cannot multiply figure by number or number by a figure")
            }
        } // TODO: Handle multiplication of numbers or figures
        case Div(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1,v2) match{
                case (NumValue(x1), NumValue(x2))=> {
                    if (x1 != 0) {NumValue(x1/x2)} else {throw new IllegalArgumentException("cannot divide by zero")}
                }
                case (FigValue(x1), FigValue(x2)) => FigValue(x1.placeTop(x2))
                case (FigValue(x1), NumValue(x2))=> FigValue(x1.rotate(x2))
                case _ => throw new IllegalArgumentException("Divide error")
            }
        } // TODO: Handle division
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => Closure(x, e, env) //TODO: Handle function definitions
        case LetRec(f, x, e1, e2) => evalExpr(e2, ExtendREC(f,x, e1, env)) // TODO: Handle recursive functions -- look at Environment.scala
        case FunCall(fCallExpr, arg) => {
            val v1 = evalExpr(fCallExpr, env)
            v1 match{
                case Closure(x, expr, env2)=> {
                    //val new_env = closed_env + (x)
                    evalExpr(expr, Extend(x, evalExpr(arg, env),env2))

                }
                case _ => throw new IllegalArgumentException("Function call error")

            }
        } // TODO: Handle function calls
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
