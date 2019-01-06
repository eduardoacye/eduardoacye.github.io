run:
	cd src && bundle exec jekyll serve

build:
	cd src && bundle exec jekyll build
	cp -r src/_site/. .
