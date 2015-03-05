---
layout: post
title:  "Lambda Calculus Talk"
date:   2015-03-03 18:00:00
categories: lambda
---

# Introduction
I'm preparing this talk for the XXV National Week of Mathematical Teaching and Investigation (XXV Semana Nacional de Investigación y Docencia en Matemáticas), the talk will cover some of the things covered in my undergraduate thesis.

The talk is about the lambda calculus. I stumbled across this formal system not in my Theoretical Computer Science classes here at "la uni", but in the magical explorations of the SICP: from [SICP](http://mitpress.mit.edu/sicp/full-text/book/book.html) to [Scheme](http://readscheme.org/) to [Lambda the Ultimate](http://library.readscheme.org/page1.html) to Lambda Calculus. I'm really fascinated with ma' homie da' lambda and my purpose of writing the thesis and doing this talk about it is to have more people get interested.

# The content of the talk (Español)

#### Introducción

El cálculo lambda es un sistema formal creado en los 30's por el matemático Alonzo Church con la finalidad de estudiar funciones. A pesar que Alonzo Church tenía grandes aspiraciones en el área de la lógica y las matemáticas, el cálculo lambda llegó a influir mas que nada en ciertas ramas de las ciencias de la computación, en particular en la computabilidad (la cual se encarga de estudiar los límites del cómputo) y en la rama de teoría de lenguajes de programación (matemáticas $$\cap$$ ciencias de la computación $$\cap$$ linguística). El sistema presenta una grán flexibilidad y extensibilidad (como plastilina) lo cual permite describirlo como un modelo de cómputo o como un lenguaje de programación.

Mi intención en esta plática es describir el cálculo lambda y exponer algunas aplicaciones e ideas interesantes que se pueden estudiar utilizandolo. Primero les presentaré la *sintaxis* y *semántica* junto con unos ejemplos para conocer el sabor de la notación; después presentaré tres "aplicaciones": la representación de la *lógica proposicional*, la *aritmética elemental* y la de *pares ordenados*; para concluír, abordaré algunas ideas básicas pero muy interesantes: *combinadores* y *recursividad*.

#### Sintaxis

Para definir el sistema formal primero se especifica como es el lenguaje usado en el cálculo lambda, este lenguaje se denota con el símbolo $$\Lambda$$, formalmente $$\Lambda$$ son llamados términos lambda.

Los términos lambda son palabras sobre el alfabeto: $$($$, $$)$$, $$\lambda$$, $$v_0$$, $$v_1$$, $$v_2$$, $$\dots$$

$$\Lambda$$ se define de la siguiente manera, considerando que $$x$$ es alguna variable:

$$
\begin{align*}
	&x \in \Lambda\\
	M \in \Lambda &\implies (\lambda x.M) \in \Lambda\\
	M, N \in \Lambda &\implies (M N) \in \Lambda
\end{align*}
$$

Ejemplos de términos lambda son:

$$z$$

$$(\lambda x. (\lambda y. (y x)) )$$

$$((\lambda y.(y y))x)$$

$$((\lambda f. (f u))(\lambda x. x))$$


#### Semántica

En el cálculo lambda hay tres tipos de términos: las variables $$x$$, las abstracciones $$(\lambda x.M)$$ y las aplicaciones $$(M N)$$. Las abstracciones son el mecanismo mediante el cual se denotan las funciones y las aplicaciones son el mecanismo mediante el cual se denota una evaluación de un término en otro.

Fuera de esto, los términos lambda no tienen un significado asociado. Mas bien define una serie de reglas de igualdad y operaciones de transformación de términos que nos permiten asociarle un significado propio.

La teoría de $$\lambda$$ tiene términos lambda $$M = N$$, determinados por las siguientes reglas:

$$
\begin{align*}
((\lambda x.M)N) &= M[x:=N]\\
M &= M\\
M=N &\implies N=M\\
M=N, N=L &\implies M=L\\
M=N &\implies MZ = ΝΖ\\
Μ=Ν &\implies ZM = ZN\\
M=N &\implies (\lambda x.M) = (\lambda x.N)
\end{align*}
$$

Además se tienen ciertas operaciones para manipular términos lambda:

- Cambio de variable ligada/Congruencia ($$\alpha$$-conversión)

> $$(\lambda x.(xz)) \equiv_{\alpha} (\lambda y.(yz))$$

- Aplicación de una función a un término ($$\beta$$-reducción)

> $$((\lambda x.(\lambda y.(xy)))(wz)) \to_{\beta} (\lambda y.((wz)y)$$

#### Emulando la lógica proposicional

- TRUE

$$(\lambda x.(\lambda y.x))$$

- FALSE

$$(\lambda x.(\lambda y.y))$$

- NOT

$$(\lambda p.(\lambda x.(\lambda y.((py)x))))$$

- AND

$$(\lambda p.(\lambda q.((pq)p)))$$

- OR

$$(\lambda p.(\lambda q.((pp)q)))$$

- IF

$$(\lambda p.(\lambda a.(\lambda b.((pa)b))))$$

#### Emulando la aritmética elemental

- Numerales de Church

$$\bar{n} \equiv (\lambda f.(\lambda x.C_n))$$

donde

$$
\begin{align*}
C_0 &\equiv x\\
C_n &\equiv (f C_{n-1})
\end{align*}
$$

- SUC

$$(λz.(λf.(λx.(f ((z f) x)))))$$

- PRE

$$(λz.(λf.(λx.(((z (λg.(λh.(h (g f))))) (λu.x)) (λu.u)))))$$

- ADD

$$(λm.(λn.((n SUC) m)))$$

- SUB

$$(λm.(λn.((n PRE) m)))$$

- MUL

$$(\lambda m.(\lambda n.(\lambda f.(m(nf)))))$$

- ZERO?

$$(\lambda n.((n(\lambda x.FALSE))TRUE))$$

- LEQ?

$$(\lambda m.(\lambda n.(ZERO?((SUB m) n))))$$

- EQ?

$$(\lambda m.(\lambda n.((AND ((LEQ? m) n))((LEQ? n)m))))$$


#### Representando pares

En las anteriores dos secciones se representaron dos tipos diferentes de valores primitivos: los números naturales y los valores de verdad. Ahora se presenta una representación para construír valores compuestos y representar estructuras mas complejas. Esto se logra representando un par de términos lambda.

- CONS

$$(\lambda x.(\lambda y.(\lambda z.((zx)y))))$$

- CAR

$$(\lambda p.(p(\lambda x.(\lambda y.x))))$$

- CDR

$$(\lambda p.(p(\lambda x.(\lambda y.y))))$$

- NIL

$$FALSE$$

- NIL?

$$(\lambda w.((w(\lambda h.(\lambda t.(\lambda d.FALSE))))TRUE))$$

#### Combinadores

Un combinador es un término lambda cerrado, es decir, que no contiene variables libres. Ejemplos de combinadores son los siguientes:

$$
\begin{equation}
(\lambda x.x)\\
(\lambda x.(\lambda y. x))\\
(\lambda x.(\lambda y.(\lambda z.((xz)(yz)))))
\end{equation}
$$

Existen combinadores bastantes interesantes, por ejemplo los combinadores de punto fijo. Un punto fijo de una función es un objeto el cual no cambia al ser aplicado en la función, en el caso del cálculo lambda, si $$P$$ es un punto fijo de $$X$$, entonces $$(XP) \to_{\beta} P$$.

Un combinador de punto fijo es un combinador $$Y$$ tal que $$(YX)\to_{\beta} (X(YX))$$ para todo término $$X$$.

Un ejemplo es el combinador de Curry-Rosenbloom:

$$(\lambda x.((\lambda y.(x(yy)))(\lambda y.(x(yy)))))$$

Estos combinadores nos permiten trabajar con funciones recursivas.

#### Ejemplo práctico (Factorial)

$$
\begin{equation*}
fact(n) = \begin{cases}
	1 & n=0\\
	n\times fact(n-1) & \text{en otro caso}
\end{cases}
\end{equation*}
$$

¿Qué tenemos a nuestra disposición en términos lambda?

- Representación de números naturales
- Representación de lógica proposicional
- Representación de funciones recursivas

Simple algoritmo:

	define factorial(n):
		si n es cero, entonces
			resultado es 1
		de lo contrario
			resultado es n*factorial(n-1)


En cálculo lambda, si consideramos a F como el término:

$$(\lambda f.(\lambda n.(((IF (ZERO? n)) 1) ((MUL n) (f (SUB n)))))))$$

Podemos expresar la evaluación de la función factorial en un número natural $n$ de la siguiente manera:

$$((YF)n)$$

# The content of the talk (English)


# The slideshow
Link to the slideshow
