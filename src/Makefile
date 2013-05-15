all:
	ghc --make main.hs -o yesod-dsl


clean:
	rm -f *.o *.hi yesod-dsl Parser.hs Lexer.hs
	happy -g Parser.y
	alex -g Lexer.x



