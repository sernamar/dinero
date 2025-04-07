# List available recipes
default:
	@just --list --unsorted

# Run the splint linter
splint:
    clojure -M:splint

# Run tests
test:
	clojure -M:test
