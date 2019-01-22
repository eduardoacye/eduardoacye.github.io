run:
	cd src && bundle exec jekyll serve

build: clean
	cd src && bundle exec jekyll build
	cp -r src/_site/. .

clean:
	rm -r 404.html 2019 about assets favicon.png feed.xml index.html tags
