# nim_parsec
# Copyright xmonader
# parsec for nim
import options, strformat, strutils, sequtils
import json

#from functools import reduce
#flatten = lambda l: [item for sublist in l for item in (sublist if isinstance(sublist, list) else [sublist] )]

type 
  EitherKind = enum
    ekLeft, ekRight
  Either = ref object
    case kind*: EitherKind 
    of ekLeft: msg*: string
    of ekRight: val*: tuple[parsed: seq[string], remaining:string]
  

proc map*(this: Either, f: proc(l:seq[string]):seq[string]): Either =
  case this.kind
  of ekLeft: return this
  of ekRight: 
    return Either(kind:ekRight, val:(parsed:f(this.val.parsed), remaining:this.val.remaining))


proc `$`*(this:Either): string =
  case this.kind
  of ekLeft: return fmt"Left {this.msg}"
  of ekRight: return fmt("<Right parsed: {this.val.parsed}, remaining: {this.val.remaining} >")

proc `==`*(this: Either, other: Either): bool =
  return this.kind == other.kind

type
  Parser = ref object
    f* : proc(s:string):Either
    suppressed*: bool
  
proc newParser(f: proc(s:string):Either, suppressed:bool=false): Parser =
  var p = Parser()
  p.suppressed = suppressed
  p.f = f 
  return p

proc `$`*(this:Parser): string =
  return fmt("<Parser:>")

proc parse*(this: Parser, s:string): Either =
  return this.f(s)


proc runParser(p: Parser, s: string): Either =
  echo $p
  return p.parse(s)

proc runParser(parsergenfn: proc():Parser, s:string): Either=
  var p = parsergenfn()
  return runparser(p, s)

 
proc map*(this:Parser, transformer:proc(l:seq[string]):seq[string]):Parser =
  proc inner(s:string):Either = 
    return this.f(s).map(transformer)
  return newParser(f=inner)


proc suppress*(this: Parser): Parser = 
    this.suppressed = true 
    return this

proc runParser*(p: Parser, inp: string): Either= 
    return p.parse(inp)

proc andThen(p1: Parser, p2: Parser): Parser =
    proc curried(s: string) : Either= 
        let res1 = p1.parse(s)
        case res1.kind
        of ekLeft:
          return res1
        of ekRight:
            let res2 = p2.parse(res1.val.remaining) # parse remaining chars.
            case res2.kind
            of ekLeft:
              return res2
            of ekRight:
                let v1 = res1.val.parsed
                let v2 = res2.val.parsed
                var vs: seq[string] = @[]
                if not p1.suppressed: #and _isokval(v1):
                    vs.add(v1) 
                if not p2.suppressed: #and _isokval(v2):
                    vs.add(v2)
                return Either(kind:ekRight, val:(parsed:vs, remaining:res2.val.remaining)) 
            return res2

    return newParser(f=curried)


proc orElse(p1, p2: Parser): Parser =
    proc curried(s: string):Either=
        let res = p1.parse(s)
        case res.kind
        of ekRight:
          return res
        of ekLeft:
          echo "S in orElse: " & s
          echo "P2 is " & $p2
          let res = p2.parse(s)
          case res.kind
          of ekLeft:
            return Either(kind:ekLeft, msg:"Failed at both")
          of ekRight:
            return res

    return newParser(curried)

proc `>>`(this: Parser, rparser:Parser): Parser =
  return andThen(this, rparser)

proc `>>`(this: Parser, parsergenfn: proc():Parser): Parser =
  return andThen(this, parsergenfn())

proc `|`(this: Parser, rparser: Parser): Parser =
  return orElse(this, rparser)


proc n(parser:Parser, count:int): Parser = 
    proc curried(s: string): Either =
        var mys = s
        var fullparsed: seq[string] = @[]
        for i in countup(1, count):
            let res = parser.parse(mys)
            case res.kind
            of ekLeft:
                return res
            of ekRight:
                let parsed = res.val.parsed
                mys = res.val.remaining
                fullparsed.add(parsed) 

        return Either(kind:ekRight, val:(parsed:fullparsed, remaining:mys))
    return newParser(f=curried)
    

proc `*`(this:Parser, times:int):Parser =
       return n(this, times) 



proc charp(c: char): Parser =
  proc curried(s:string):Either =
      if s.isNilOrEmpty():
          let msg = "S is empty"
          return Either(kind:ekLeft, msg:msg)
      else:
          if s[0] == c:
            let rem = s[1..<s.len]
            let parsed_string = @[$c]
            return Either(kind:ekRight, val:(parsed:parsed_string, remaining:rem))
          else:
              return Either(kind:ekLeft, msg:fmt"Expecting '${c}' and found '{s[0]}'")
  return newParser(curried)

proc choice*(parsers: seq[Parser]): Parser = 
    return foldl(parsers, a | b)

proc anyOf(chars: set[char]): Parser =
    return choice(mapIt(chars, charp(it)))

proc parseString(s:string): Parser =
  var parsers: seq[Parser] = newSeq[Parser]()
  for c in s:
    parsers.add(charp(c))
  var p = foldl(parsers, a >> b)
  return p.map(proc(l:seq[string]):seq[string] = @[join(l, "")])

proc until_seq(s:string): Parser = 
    proc curried(inp:string):Either =
        if inp.isNilOrEmpty():
            let msg = "S is empty"
            return Either(kind:ekLeft, msg:msg)
        else:
            if s == inp[0..<s.len]:
              let myparsed = @[""]
              return Either(kind:ekRight, val:(parsed:myparsed, remaining:s))
            else:
                return Either(kind:ekLeft, msg:"Expecting '{s}' and found '{inp[0..<s.len]}'")
    return newParser(f=curried)

proc until(p:Parser): Parser =
    proc curried(s:string):Either =
        let res = p.parse(s)
        case res.kind
        of ekLeft: return res
        of ekRight:
          let myparsed = @[""]
          return Either(kind:ekRight, val:(parsed:myparsed, remaining:s))
    return newParser(f=curried)

let chars = parseString

proc parseZeroOrMore(parser: Parser, inp:string): Either = #zero or more
    let res = parser.parse(inp)
    echo "RES :" & $res
    case res.kind
    of ekLeft:
      let myparsed: seq[string] = @[]
      return Either(kind:ekRight, val:(parsed:myparsed, remaining:inp))
    of ekRight:
      let firstval = res.val.parsed
      let restinpafterfirst = res.val.remaining
      # echo "REST INP AFTER FIRST " & restinpafterfirst
      let res = parseZeroOrMore(parser, restinpafterfirst)
      case res.kind
      of ekRight:
        let subseqvals = res.val.parsed
        let remaining = res.val.remaining
        var values:seq[string] = newSeq[string]()
        # echo "FIRST VAL: " & firstval
        # echo "SUBSEQ: " & $subseqvals
        values.add(firstval)
        values.add(subseqvals)
        return Either(kind:ekRight, val:(parsed:values, remaining:remaining))
      of ekLeft:
        let myparsed: seq[string] = @[]
        return Either(kind:ekRight, val:(parsed:myparsed, remaining:inp))
      
proc many(parser:Parser):Parser =
    proc curried(s: string): Either =
        return parse_zero_or_more(parser,s)
    
    ## Q: should we enable this by default?
    # let transformer = proc(l:seq[string]):seq[string]= @[join(l, "")]
    return newParser(f=curried) #.map(transformer)


proc many1(parser:Parser): Parser =
    proc curried(s: string): Either =
        let res = parser.parse(s)
        case res.kind
        of ekLeft:
          return res
        of ekRight:
          return many(parser).parse(s)
    return newParser(f=curried)


proc optionally(parser: Parser): Parser =
    let myparsed = @[""]
    let nonproc = proc(s:string):Either = Either(kind:ekRight, val:(parsed:myparsed, remaining:""))
    let noneparser = newParser(f=nonproc)
    return parser | noneparser

proc sep_by1(sep: Parser, parser:Parser): Parser =
    let sep_then_parser = sep >> parser
    echo "sepby1 is called again"
    return (parser >> many(sep_then_parser))

proc sep_by(sep: Parser, parser:Parser): Parser =
  let myparsed = @[""]
  let nonproc = proc(s:string):Either = Either(kind:ekRight, val:(parsed:myparsed, remaining:""))
  return (sep_by1(sep, parser) | newParser(f=nonproc))

proc forward(parsergeneratorfn: proc():Parser): proc(s: string): Either =
  proc curried(s: string): Either =
    let p: Parser = parsergeneratorfn()
    return p.parse(s)
  return curried

let smashtransformer = proc(l:seq[string]):seq[string]= @[join(l, "")]

let letter = anyOf(strutils.Letters)
let lletter = anyOf({'a'..'z'})
let uletter = anyOf({'A'..'Z'})
let digit = anyOf(strutils.Digits)
let hexdigit, hexstr = anyOf(strutils.HexDigits)
let ws = anyOf(strutils.Whitespace)
let letters = many1(letter)
let digits = many1(digit)
let word = letters.map(smashtransformer)
let alphanumword = many1(letter >> (letters|digits)).map(smashtransformer)
let between = proc(p1, p2, p3: Parser): Parser =
    return p1 >> p2 >> p3
let surroundedBy = proc(surparser, contentparser: Parser): Parser =
    return surparser >> contentparser >> surparser
let option = optionally


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
var valp = forward(proc():Parser =digits|listp)
listp = charp('[') >> sep_by(charp(',').suppress(), many1(valp)) >> charp(']')
valp = listp|digits

echo $valp.parse("1")
echo "DONE"
echo $valp.parse("[1,2]")
# echo $valp.parse("[1,[1,2]]")