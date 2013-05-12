all:
	ghc --make main.hs -o yesod-generate-rest

Parser.hs: Parser.y
	happy -g Parser.y

Lexer.hs: Lexer.x    
	alex -g Lexer.x

clean:
	rm -f *.o *.hi yesod-generate-rest Parser.hs Lexer.hs
	happy -g Parser.y
	alex -g Lexer.x



