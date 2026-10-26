set shell := ["nu", "-c"]

serve: build
    npx -y serve dist -l 8080

index:
    ls recipes/*.rcp | get name | path basename | sort | to json | save --force recipes.json

build: index
    if ("dist" | path exists) { rm --recursive dist }
    mkdir dist/recipes
    npx -y elm@0.19.2-0 make src/Main.elm --optimize --output=dist/index.html
    cp recipes.json dist/recipes.json
    cp recipes/*.rcp dist/recipes/
