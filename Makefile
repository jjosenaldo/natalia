# Paths 
LEX_PATH = Lexical
PARSER_PATH = Parsers

# extensions
LEX_EXT = x

# compiler 
LC = ghc

# tokenizer
LEXER = alex

# executable
BIN_NAME = natalia.out

# Find all source files in the source directory, sorted by
# most recently modified
LEX_FILES = $(shell find $(LEX_PATH) -name '*.$(LEX_EXT)' | sort -k 1nr | cut -f2-)

all: tokenizer generate

# execute the alex in all archive .x
.PHONY: tokenizer
tokenizer: 
	@echo "reading tokens..."
	$(LEXER) $(LEX_FILES)

# generate a executable
.PHONY: generate
generate:
	@echo "compiling..."
	$(LC) $(PARSER_PATH)/Main.hs -o $(BIN_NAME)

.PHONY: clean
clean: 
	@echo "\nCleaning up..."
	@rm -rf $(LEX_PATH)/*.o
	@rm -rf $(LEX_PATH)/*.hi
	@rm -rf $(LEX_PATH)/*.hs
	@rm -rf $(PARSER_PATH)/*.o
	@rm -rf $(PARSER_PATH)/*.hi
	
