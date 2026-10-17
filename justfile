set shell := ["nu", "-c"]

serve:
    npx -y serve dist -l 8080

index:
    ls recipes/*.rcp | get name | path basename | sort | to json | save --force recipes.json

build: index
    npx -y elm@0.19.2-0 make src/Main.elm --optimize --output=index.html

deploy:
    git pull --ff-only
    just build
    mkdir dist/recipes
    cp index.html dist/index.html
    cp recipes.json dist/recipes.json
    cp recipes/*.rcp dist/recipes/
    just serve
