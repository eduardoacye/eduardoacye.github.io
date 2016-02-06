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

### Compilador e interprete de Lisp

El dialecto de Lisp en el que programaremos será **Common Lisp** y la implementación [SBCL](http://www.sbcl.org/).

### Magia arcana con SLIME
