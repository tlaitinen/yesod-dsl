all: test

test: test.hs DbLexer.hs DbParser.hs ModuleMerger.hs IfaceImplementer.hs NameFinder.hs DbTypes.hs Generator.hs SyncFiles.hs
	ghc --make test.hs

DbLexer.hs: DbLexer.x
	alex -g DbLexer.x

DbParser.hs: DbParser.y
	happy -g DbParser.y

clean:
	rm -f *.o *.hi test
