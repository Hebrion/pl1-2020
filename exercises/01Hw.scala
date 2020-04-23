/**
Homework 01
============
Before doing anything else, register for the Programmiersprachen 1 exercises
by sending an email to Julian Jabs:

julian.jabs@uni-tuebingen.de

Write in the email:
- name
- Matrikelnummer
- Studiengang + Abschluss (BSc, MSc, ...)
- Fachsemester
- name of your GitHub account (register under github.com if you do not have one yet)
- optionally: programming languages you already know

You will receive an invitation to our forum for Programmiersprachen 1.
In the "Organisatorisches" post you will find everything necessary
to get you started. Read that post first and follow the instructions.

In particular, in that post you will find an invitation to the GitHub Education
platform for Programmiersprachen 1 exercises.

Work in groups of 1 or 2 students.
If you want to work together with another student,
have one of you create a team (you will be asked
to create or join a team by default) and the other
then join that team.

If you have any questions regarding these processes,
just ask in the forum beneath the "Organisatorisches" post.

Submit your solution to this exercise until Friday, 24.4., 10:00h
via the GitHub repo for your team and for this exercise.
*/

object Hw01 {


//Impl geht leider nicht. "may be exhaustive"


sealed abstract class Exp
case class True() extends Exp  // constant true
case class False() extends Exp // constant false
case class And(lhs: Exp, rhs: Exp) extends Exp
case class Or(lhs: Exp, rhs: Exp) extends Exp
case class Not(e: Exp) extends Exp
case class Impl(lhs:Exp, rhs: Exp) extends Exp

def eval(e: Exp) : Boolean = e match {
  case True() => true
  case False() => false
  case And(lhs, rhs) => eval(lhs) && eval (rhs)
  case Or(lhs, rhs) => eval(lhs) || eval (rhs)
  case Not(e)  => !(eval(e))
  case Impl(lhs, rhs) => if ((eval(lhs) == true) && (eval(rhs) == false)){
    false
  }else{
    true
  }
}

val exampleProposition1 = And(Not(True()), False()) // should evaluate to false
val exampleProposition2 = Or(True(), False())
val exampleProposition3 = And(Not(True()), Or(True(), False()))

val exampleProposition4 = Impl(True(), And(Not(True()), Or(True(), False())))
val exampleProposition5 = Impl(False(), True())

eval(exampleProposition1)
eval(exampleProposition2)
eval(exampleProposition3)



}
