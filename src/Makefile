all: yesod-generate-rest

yesod-generate-rest: yesod-generate-rest.hs DbLexer.hs DbParser.hs ModuleMerger.hs IfaceImplementer.hs NameFinder.hs DbTypes.hs Generator.hs SyncFiles.hs CheckFieldNames.hs 
	ghc --make yesod-generate-rest.hs

DbLexer.hs: DbLexer.x
	alex -g DbLexer.x

DbParser.hs: DbParser.y
	happy -g DbParser.y

clean:
	rm -f *.o *.hi yesod-generate-rest
