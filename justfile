set shell := ["nu", "-c"]

serve:
    npx -y serve . -l 8080

index:
    ls recipes/*.rcp | get name | path basename | sort | to json | save --force recipes.json

build: index
    npx -y elm@0.19.2-0 make src/Main.elm --optimize --output=index.html
