.PHONY: clean distclean default

LLVMCONFIG=llvm-config

CXX=g++
CXXFLAGS=-g `$(LLVMCONFIG) --cxxflags` -std=c++23 -fexceptions
LDFLAGS=`$(LLVMCONFIG) --ldflags --system-libs --libs all`

default: gracec

lexer.cpp: lexer.l
	flex -s -o lexer.cpp lexer.l

lexer.o: lexer.cpp lexer.hpp parser.hpp ast.hpp symbol.hpp

parser.hpp parser.cpp: parser.y
ifeq ($(MAKEFLAGS),e)
	bison -dv -Wcounterexamples -o parser.cpp parser.y
else
	bison -dv -o parser.cpp parser.y
endif

parser.o: parser.cpp lexer.hpp symbol.hpp ast.hpp

gracec: lexer.o parser.o ast.o
	$(CXX) $(CXXFLAGS) -o gracec $^ $(LDFLAGS)

clean:
	$(RM) lexer.cpp parser.cpp parser.hpp parser.output *.o *.s *.ll

distclean: clean
	$(RM) gracec *.out*
