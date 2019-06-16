# Paths 
BLK_PATH = Blocks
EXP_PATH = Expressions
NEWEXP_PATH = NewExpressions
LEX_PATH = Lexical
MEM_PATH = Memory
STMT_PATH = Statements
SYTX_PATH = Syntax
TYPE_PATH = Types
TV_PATH = TypeValue

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
	$(LC) Main.hs -o $(BIN_NAME) 

.PHONY: clean
clean: 
	@echo "\nCleaning up..."
	@rm -rf *.o *.hi
	@rm -rf $(BLK_PATH)/*.o
	@rm -rf $(BLK_PATH)/*.hi
	@rm -rf $(EXP_PATH)/*.o
	@rm -rf $(EXP_PATH)/*.hi
	@rm -rf $(NEWEXP_PATH)/*.o
	@rm -rf $(NEWEXP_PATH)/*.hi
	@rm -rf $(LEX_PATH)/*.o
	@rm -rf $(LEX_PATH)/*.hi
	@rm -rf $(MEM_PATH)/*.o
	@rm -rf $(MEM_PATH)/*.hi
	@rm -rf $(STMT_PATH)/*.o
	@rm -rf $(STMT_PATH)/*.hi
	@rm -rf $(SYTX_PATH)/*.o
	@rm -rf $(SYTX_PATH)/*.hi
	@rm -rf $(TYPE_PATH)/*.o
	@rm -rf $(TYPE_PATH)/*.hi
	@rm -rf $(TV_PATH)/*.o
	@rm -rf $(TV_PATH)/*.hi