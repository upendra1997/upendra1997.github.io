sudo hugo -D -b "https://www.hdggx.in/" -d /var/www/hdggx.in --gc --minify
rsync -avz /var/www/hdggx.in/ aws:/var/www/hdggx.in/
