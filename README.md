# nim-parsec
parsec for nim


## example 

```

  let aParser = charp('a')
  let bParser = charp('b')
  echo $aParser.parse("abc")
  echo $bParser.parse("bca")

  let abParser = charp('a') >> charp('b')
  echo $abParser.parse("abc")

  let aorbParser = charp('a') | charp('b')
  echo $aorbParser.parse("acd")
  echo $aorbParser.parse("bcd")

  let abcParser = parseString("abc")
  echo $abcParser.parse("abcdef")

  let manyA = many(charp('a'))
  echo $manyA.parse("aaab")
  echo $manyA.parse("bbb")

  let manyA1 = many1(charp('a'))
  echo $manyA1.parse("aaab")
  echo $manyA1.parse("bbb")

  let manyDigits = many1(digit)
  echo $manyDigits.parse("1234")

  let commaseparatednums = sep_by(charp(',').suppress(), digit)
  echo $commaseparatednums.parse("1,2,4")

  #let greetparser = letters >> charp(',') >> many(ws) >> letters
  let greetparser = word >> charp(',').suppress() >> many(ws).suppress() >> word
  echo $greetparser.parse("Hello,   World")

  echo $(letter*3).parse("abc")

  let uuidsample = "db9674c4-72a9-4ab9-9ddd-1d641a37cde4"
  let uuidparser =(hexstr*8).map(smashtransformer) >> charp('-') >> (hexstr*4).map(smashtransformer) >> charp('-') >>  (hexstr*4).map(smashtransformer) >> charp('-') >> (hexstr*4).map(smashtransformer) >> charp('-') >> (hexstr*12).map(smashtransformer)
  echo $uuidparser.parse(uuidsample)


  # recursive lang ints and list of ints or lists
  var listp: Parser
  var valref = (proc():Parser =digits|listp)
  listp = charp('[') >> sep_by(charp(',').suppress(), many(valref)) >> charp(']')
  var valp = valref()

  echo $valp.parse("1")
  echo $valp.parse("[1,2]")
  echo $valp.parse("[1,[1,2]]")

```

```
<Right parsed: @["a"], remaining: bc >
<Right parsed: @["b"], remaining: ca >
<Right parsed: @["a", "b"], remaining: c >
<Right parsed: @["a"], remaining: cd >
<Right parsed: @["b"], remaining: cd >
<Right parsed: @["abc"], remaining: def >
<Right parsed: @["a", "a", "a"], remaining: b >
<Right parsed: @[], remaining: bbb >
<Right parsed: @["a", "a", "a"], remaining: b >
Left Expecting '$a' and found 'b'
<Right parsed: @["1", "2", "3", "4"], remaining:  >
<Right parsed: @["1", "2", "4"], remaining:  >
<Right parsed: @["Hello", "World"], remaining:  >
<Right parsed: @["a", "b", "c"], remaining:  >
<Right parsed: @["db9674c4", "-", "72a9", "-", "4ab9", "-", "9ddd", "-", "1d641a37cde4"], remaining:  >
<Right parsed: @["1"], remaining:  >
<Right parsed: @["[", "1", "2", "]"], remaining:  >
<Right parsed: @["[", "1", "[", "1", "2", "]", "]"], remaining:  >
```