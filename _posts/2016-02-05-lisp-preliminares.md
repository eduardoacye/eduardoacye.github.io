---
layout: post
title: "Preliminares de Lisp"
date: 2016-02-05
---

## Instalación del entorno de desarrollo

Para poder programar en Lisp con comodidad se necesita tener un entorno de desarrollo. Tener un entorno de desarrollo configurado para la programación en Lisp nos brindará *coloración de sintaxis*, *completado de paréntesis*, *edición estructural*, *interacción con REPL*, entre otras cosas.

Todo el proceso de instalación y configuración se realiza en una máquina virtual con Debian, si tienes Mac o Windows y no puedes/quieres instalar en tu computadora alguna variante de Linux te recomiendo utilizar una máquina virtual o buscar tutoriales en internet.

![](https://www.debian.org/Pics/openlogo-50.png)

### Editor de código para Lisp

Varios editores de texto pueden ser utilizados con Lisp, dependiendo del dialecto y la implementación existen configuraciones especializadas para varios editores.

En esta ocación se describirá el uso de [Emacs](http://gnu.org/s/emacs), sin embargo, otros editores e IDEs pueden ser configurados para programar en Lisp (Si Emacs no te late chocolate, puedes buscar tutoriales para programar Lisp en [Vim](http://www.vim.org/), [Sublime](https://www.sublimetext.com/), [Atom](https://atom.io/), entre otros).

![]({{site.url}}/assets/emacs-logo.png)

#### Instalar Emacs

Dirígete al sitio web [http://gnu.org/s/emacs](http://gnu.org/s/emacs) y descarga la última versión de Emacs (En el momento en que escribo este post es la versión 24.5) visitando el link de *obtaining* en la sección *Releases* de la página. El archivo que descargué fué `emacs-24.5.tar.xz` de aproximadamente 38 megas. Cuando finalice la descarga compila e instala Emacs.

Si prefieres no compilarlo tu mismo hay varias versiones pre-compiladas en los repositorios de la mayoría (si no es que todas) las distribuciones de Linux (si estás en una Mac utiliza ports o homebrew, si estás en Windows bája una versión pre-compilada buscando en google).

Ya que tengas instalado Emacs puedes abrirlo y verás una ventana como la siguiente

![]({{site.url}}/assets/emacs-start2.png)

Ahora descarga [esta]({{site.url}}/assets/config.el) configuración de emacs y copia el su contenido en el archivo de configuración de emacs. Para saber cuál es este archivo abre emacs y presiona `M-:` (en la jerga esto se lee "meta y dos puntos" lo cual significa presionar la tecla `Alt` (usualmente la tecla meta es la tecla alt), después presionar `:` y después soltar ambas teclas) en la parte baja de la ventana de Emacs escribe user-init-file y después presiona la tecla `Enter`. Se verá algo así:

![]({{site.url}}/assets/emacs-user-init-file2.png)

Usualmente será un archivo en el directorio del usuario llamado `.emacs`.

Cierra Emacs y vuelve a abrirlo, es necesario que cuentes con una conexión a internet porque se descargarán algunos paquetes de Emacs.

Cuando este proceso termine, reinicia Emacs de nuevo y verás algo similar a la siguiente ventana:

![]( {{site.url}}/assets/emacs-configured2.png )

A primera vista, no mucho ha cambiado del editor, la configuración quitó la barra de menús y la barra de herramientas, así como las barras de desplazamiento. Además se instalaron los paquetes `ido-ubiquitous`, `smex` y `paredit`.

Te sugiero que sigas el tutorial de Emacs que viene incluído en el programa. Estando en Emacs puedes escribir `C-h t` (Presionar `Ctrl` y `h` seguido de `t`) para iniciarlo.

Cierra Emacs por el momento y prepárate instalar un Lisp.

### Compilador e interprete de Lisp

El dialecto de Lisp en el que programaremos será **Common Lisp** y la implementación [Steel Bank Common Lisp](http://www.sbcl.org/) (abreviado SBCL).

Dirígete al sitio de la implementación, descarga un binario para tu plataforma y sigue las instrucciones que vienen en la sección de *getting started*.

Cuando termines ese proceso tendrás un ejecutable llamado `sbcl`, el cual al correrlo verás una pantalla como la siguiente:

![]( {{site.url}}/assets/sbcl-terminal2.png)

### Quicklisp

Quicklisp es un gestor de bibliotecas para Common Lisp y permite descargar bibliotecas bien pelada.

Dirígete al sitio web de [Quicklisp](https://www.quicklisp.org/beta/) y sigue los pasos de instalación.

### SLIME

El SLIME (Superior Lisp Interaction Mode for Emacs) es un modo de Emacs para programar en Common Lisp.

Para instalarlo en `sbcl` escribe `(ql:quickload "quicklisp-slime-helper")` y presiona enter.

Cuando termine el proceso, un mensaje aparecerá que se ve similar a lo siguiente:

![]( {{site.url}}/assets/quicklisp-slime2.png )

Copia las lineas de código que aparecen en el mensaje y escríbelas en el archivo de configuración de Emacs en la sección *COMMON LISP*, en mi caso se ve así:

![]( {{site.url}}/assets/emacs-slime2.png )

### Siguientes pasos

#### Edición estructural de código

Una característica de la familia de lenguajes Lisp es que el código tiene forma de árbol, similar a un árbol de sintáxis obtenido después de parsear un lenguaje de programación.


#### Programación interactiva con el REPL

#### Referencias bibliográficas

---

Prepárate para invocar conjuros arcanos en la siguiente entrada del blog.
