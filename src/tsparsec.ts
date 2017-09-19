import { Optional, Array1, mixed, CodePoint } from "wiinuk-extensions"
import * as ex from "wiinuk-extensions"


export type DefaultElement = CodePoint

export interface BufferLike<E> extends Iterable<E> {
    readonly length: number
    readonly [index: number]: E
    slice(start: number, end: number): this
}

export interface StreamState {
    position: number
    line: number
    column: number

    errorPosition: number
    errorLine: number
    errorColumn: number
}
export const enum SnapshotTag { Ok, Error }
export interface OkSnapshot<T> extends StreamState {
    readonly tag: SnapshotTag.Ok
    readonly value: T
}
export interface ErrorSnapshot extends StreamState {
    readonly tag: SnapshotTag.Error
    readonly message: string
}
export type Snapshot<T> =
    | OkSnapshot<T>
    | ErrorSnapshot

export type Memo<T> = Map<number, Snapshot<T>>
export type Cache = { [key: string]: Optional<Memo<any>> }

export interface Stream<E> extends StreamState {
    readonly source: string | null
    readonly buffer: BufferLike<E>
    readonly length: number
    readonly cache: Cache
    advance(element: E): void
}
export type Parser<E, T> = (stream: Stream<E>) => T

export interface Combinator<E, T> {
    readonly parser: Parser<E, T>

    many0(): Combinator<E, T[]>
    many1(): Combinator<E, Array1<T>>
    skipMany0(): Combinator<E, null>
    skipMany1(): Combinator<E, null>
    many1Fold<U>(init: () => U, folder: (state: U, value: T) => U): Combinator<E, U>
    sepBy0<U>(sep: Combinator<E, U>): Combinator<E, T[]>
    sepBy1<U>(sep: Combinator<E, U>): Combinator<E, Array1<T>>
    skipSepBy1<U>(sep: Combinator<E, U>): Combinator<E, null>

    left<U>(right: Combinator<E, U>): Combinator<E, T>
    right<U>(right: Combinator<E, U>): Combinator<E, U>
    notFollowedBy(): Combinator<E, null>
    map<U>(mapping: (value: T) => U): Combinator<E, U>
    opt(): Combinator<E, Optional<T>>

    // ```F#
    //for i in 0..15 do
    //    let m f = {1..i} |> Seq.map ((+) 1 >> f) |> String.concat "" 
    //    let f = sprintf >> m
    //    printfn "    pipe<%sU>(%smapping: (x1: T%s) => U): Combinator<E, U>"
    //        (f "T%d, ") (m <| fun n -> sprintf "parser%d: Combinator<E, T%d>, " n n)
    //        (m <| fun n -> sprintf ", x%d: T%d" n n)
    // ```
    pipe<U>(mapping: (x1: T) => U): Combinator<E, U>
    pipe<T2, U>(parser2: Combinator<E, T2>, mapping: (x1: T, x2: T2) => U): Combinator<E, U>
    pipe<T2, T3, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, mapping: (x1: T, x2: T2, x3: T3) => U): Combinator<E, U>
    pipe<T2, T3, T4, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, mapping: (x1: T, x2: T2, x3: T3, x4: T4) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, T10, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, parser10: Combinator<E, T10>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, parser10: Combinator<E, T10>, parser11: Combinator<E, T11>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, parser10: Combinator<E, T10>, parser11: Combinator<E, T11>, parser12: Combinator<E, T12>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, parser10: Combinator<E, T10>, parser11: Combinator<E, T11>, parser12: Combinator<E, T12>, parser13: Combinator<E, T13>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, parser10: Combinator<E, T10>, parser11: Combinator<E, T11>, parser12: Combinator<E, T12>, parser13: Combinator<E, T13>, parser14: Combinator<E, T14>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, parser10: Combinator<E, T10>, parser11: Combinator<E, T11>, parser12: Combinator<E, T12>, parser13: Combinator<E, T13>, parser14: Combinator<E, T14>, parser15: Combinator<E, T15>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15) => U): Combinator<E, U>
    pipe<T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, U>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>, parser5: Combinator<E, T5>, parser6: Combinator<E, T6>, parser7: Combinator<E, T7>, parser8: Combinator<E, T8>, parser9: Combinator<E, T9>, parser10: Combinator<E, T10>, parser11: Combinator<E, T11>, parser12: Combinator<E, T12>, parser13: Combinator<E, T13>, parser14: Combinator<E, T14>, parser15: Combinator<E, T15>, parser16: Combinator<E, T16>, mapping: (x1: T, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16) => U): Combinator<E, U>

    // ```F#
    // for i in 0..15 do
    //     let g sep f = {1..i} |> Seq.map ((+) 1 >> f) |> String.concat sep
    //     printfn "    tuple%s(%s): Combinator<E, [T%s]>"
    //         (if i = 0 then "" else sprintf "<%s>" <| g ", " (sprintf "T%d"))
    //         (g ", " <| fun n -> sprintf "parser%d: Combinator<E, T%d>" n n)
    //         (g "" <| sprintf ", T%d")
    // ```
    tuple(): Combinator<E, [T]>
    tuple<T2>(parser2: Combinator<E, T2>): Combinator<E, [T, T2]>
    tuple<T2, T3>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>): Combinator<E, [T, T2, T3]>
    tuple<T2, T3, T4>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>): Combinator<E, [T, T2, T3, T4]>

    return<U>(value: U): Combinator<E, U>

    skipped(this: Combinator<CodePoint, T>): Combinator<CodePoint, string>
    manyChars0(this: Combinator<E, CodePoint>): Combinator<E, string>

    run(this: Combinator<E, T>, source: ReadonlyArray<E>): T
    runString(this: Combinator<CodePoint, T>, string: string, source?: string | null): T
}

export function error<E>(message: string, stream: Stream<E>, position = stream.position, line = stream.line, column = stream.column): never {
    if (stream.errorPosition < position) {
        stream.errorPosition = position
        stream.errorLine = line
        stream.errorColumn = column
    }
    throw message
}

function parseOpt<E, T>(parser: Parser<E, T>, stream: Stream<E>) {
    const { position, line, column, errorPosition, errorLine, errorColumn } = stream
    try {
        const result = parser(stream)
        return (position === stream.position) ? void 0 : result
    }
    catch {
        stream.position = position
        stream.line = line
        stream.column = column
        stream.errorPosition = errorPosition
        stream.errorLine = errorLine
        stream.errorColumn = errorColumn
        return void 0
    }
}

function map<E, T, U>(p: Parser<E, T>, mapping: (value: T) => U): Parser<E, U> {
    return stream => mapping(p(stream))
}
function opt<E, T extends {} | null>(p: Parser<E, T>): Parser<E, Optional<T>> {
    return stream => parseOpt(p, stream)
}

function left<E, T1, T2>(p1: Parser<E, T1>, p2: Parser<E, T2>): Parser<E, T1> {
    return stream => { const x1 = p1(stream); p2(stream); return x1 }
}
function right<E, T1, T2>(p1: Parser<E, T1>, p2: Parser<E, T2>): Parser<E, T2> {
    return stream => (p1(stream), p2(stream))
}
function pipeAny<E, T, U>(parsers: ArrayLike<Parser<E, T>>, mapping: (...xs: T[]) => U): Parser<E, U> {
    const results: T[] = []
    return stream => {
        for (let i = 0; i < parsers.length; i++) { results[i] = parsers[i](stream) }
        return mapping(...results)
    }
}
function tupleAny<E, T>(parsers: ReadonlyArray<Parser<E,T>>): Parser<E, T[]> {
    return stream => parsers.map(p => p(stream))
}
function pipe<E, T1, T2, U, S, R1,R2>(p1: Parser<E, T1>, p2: Parser<E, T2>, mapping: (x1: T1, x2: T2) => U): Parser<E, U>
function pipe<E, T1, T2, T3, U, S, R1,R2,R3>(p1: Parser<E, T1>, p2: Parser<E, T2>, p3: Parser<E, T3>, mapping: (x1: T1, x2: T2, x3: T3) => U): Parser<E, U>
function pipe<E, T1, T2, T3, T4, U, S, R1,R2,R3,R4>(p1: Parser<E, T1>, p2: Parser<E, T2>, p3: Parser<E, T3>, p4: Parser<E, T4>, mapping: (x1: T1, x2: T2, x3: T3, x4: T4) => U): Parser<E, U>
function pipe<E, T, U>(...parserOrMappings: (Parser<E, T> | ((...xs: T[]) => U))[]): Parser<E, U> {
    const mapping = parserOrMappings.pop() as (...xs: T[]) => U
    const parsers = parserOrMappings as Parser<E, T>[]
    return pipeAny(parsers, mapping)
}

function skipMany0Parse<E, T>(stream: Stream<E>, parser: Parser<E, T>) {
    while (true) {
        if (parseOpt(parser, stream) === void 0) { return null }
    }
}

function many0<E, T>(parser: Parser<E, T>): Parser<E, T[]> {
    return stream => {
        const result: T[] = []
        while (true) {
            const x = parseOpt(parser, stream)
            if (x === void 0) { return result }
            result.push(x)
        }
    }
}
function many1<E, T>(parser: Parser<E, T>): Parser<E, Array1<T>> {
    return stream => {
        const result: Array1<T> = [parser(stream)]
        while (true) {
            const x = parseOpt(parser, stream)
            if (x === void 0) { return result }
            result.push(x)
        }
    }
}
function many1Fold<E, T, U>(parser: Parser<E, T>, init: () => U, folder: (state: U, value: T) => U): Parser<E, U> {
    return stream => {
        let state = init()
        while (true) {
            const x = parseOpt(parser, stream)
            if (x === void 0) { return state }
            state = folder(state, x)
        }
    }
}
function skipMany0<E, T>(parser: Parser<E, T>): Parser<E, null> {
    return stream => skipMany0Parse(stream, parser)
}
function skipMany1<E, T>(parser: Parser<E, T>): Parser<E, null> {
    return stream => (parser(stream), skipMany0Parse(stream, parser))
}
function sepBy1<E, T, U>(parser: Parser<E, T>, sep: Parser<E, U>): Parser<E, Array1<T>> {
    return pipe(parser, many0(right(sep, parser)), (x, xs) => (xs.unshift(x), xs as Array1<T>))
}
function sepBy0<E, T, U>(parser: Parser<E, T>, sep: Parser<E, U>): Parser<E, T[]> {
    return map(opt(sepBy1(parser, sep)), xs => (xs === void 0) ? [] : xs)
}
function skipSepBy1<E, T, U>(parser: Parser<E, T>, sep: Parser<E, U>): Parser<E, null> {
    return right(parser, skipMany0(right(sep, parser)))
}

function notFollowedBy<E, T>(parser: Parser<E, T>, label = "notFollowedBy"): Parser<E, null> {
    return stream => (parseOpt(parser, stream) === void 0) ? null : error(label, stream)
}
function choiceParsers<E, T>(parsers: ReadonlyArray<Parser<E, T>>): Parser<E, T> {
    return stream => {
        let { position: maxPosition, line: maxLine, column: maxColumn } = stream
        let maxError = ""

        for (let i = 0; i < parsers.length; i++) {
            const { position, line, column, errorPosition, errorLine, errorColumn } = stream
            try {
                return parsers[i](stream)
            }
            catch (error) {
                const { position: parsePosition, line: parseLine, column: parseColumn } = stream
                if (maxPosition <= parsePosition) {
                    maxPosition = parsePosition
                    maxLine = parseLine
                    maxColumn = parseColumn
                    maxError = String(error)
                }
                stream.position = position
                stream.line = line
                stream.column = column
                stream.errorPosition = errorPosition
                stream.errorLine = errorLine
                stream.errorColumn = errorColumn
            }
        }
        return error(maxError, stream, maxPosition, maxLine, maxColumn)
    }
}
function skipped<T>(parser: Parser<CodePoint, T>): Parser<CodePoint, string> {
    return stream => {
        const { buffer: b, position: p } = stream
        parser(stream)
        const ep = stream.position
        switch (ep - p) {
            case 0: return ""
            case 1: return String.fromCodePoint(b[p])
            case 2: return String.fromCodePoint(b[p], b[p + 1])
            case 3: return String.fromCodePoint(b[p], b[p + 1], b[p + 2])
            case 4: return String.fromCodePoint(b[p], b[p + 1], b[p + 2], b[p + 3])
            case 5: return String.fromCodePoint(b[p], b[p + 1], b[p + 2], b[p + 3], b[p + 4])
            case 6: return String.fromCodePoint(b[p], b[p + 1], b[p + 2], b[p + 3], b[p + 4], b[p + 5])
            case 7: return String.fromCodePoint(b[p], b[p + 1], b[p + 2], b[p + 3], b[p + 4], b[p + 5], b[p + 6])
            case 8: return String.fromCodePoint(b[p], b[p + 1], b[p + 2], b[p + 3], b[p + 4], b[p + 5], b[p + 6], b[p + 7])
            case 9: return String.fromCodePoint(b[p], b[p + 1], b[p + 2], b[p + 3], b[p + 4], b[p + 5], b[p + 6], b[p + 7], b[p + 8])
            case 10: return String.fromCodePoint(b[p], b[p + 1], b[p + 2], b[p + 3], b[p + 4], b[p + 5], b[p + 6], b[p + 7], b[p + 8], b[p + 9])
            default: return String.fromCodePoint(...b.slice(p, ep))
        }
    }
}

function manyChars0<E>(parser: Parser<E, CodePoint>): Parser<E, string> {
    return stream => {
        const chars = []
        while (true) {
            const char = parseOpt(parser, stream)
            if (char === void 0) { return String.fromCodePoint(...chars) }
            chars.push(char)
        }
    }
}

abstract class StreamClass<E> implements Stream<E> {
    readonly length: number
    position = 0
    line = 1
    column = 1
    errorPosition = -1
    errorLine = -1
    errorColumn = -1
    readonly cache: Cache = Object.create(null)
    constructor(
        public readonly buffer: BufferLike<E>,
        public readonly source: string | null = null
    ) {
        this.length = buffer.length
    }
    abstract advance(element: E): void
}
class ArrayStream<E> extends StreamClass<E> { advance() { this.position++ } }
class CharStream extends StreamClass<CodePoint> {
    constructor(string: string, source: string | null = null) { super(ex.String.codePoints(string), source) }
    advance(char: CodePoint) {
        this.position++
        if (char === CodePoint["\n"]) {
            this.line++
            this.column = 1
        }
        else {
            this.column++
        }
    }
}

function runStream<E, T>(parser: Parser<E,T>, stream: Stream<E>) {
    try {
        return parser(stream)
    }
    catch (error) {
        throw `(${stream.errorLine}, ${stream.errorColumn}): ${error}`
    }
}
function run<E, T>(parser: Parser<E, T>, array: ReadonlyArray<E>, source: string | null = null) {
    return runStream(parser, new ArrayStream<E>(array, source))
}

function runString<T>(parser: Parser<CodePoint, T>, string: string, source: string | null = null) {
    return runStream(parser, new CharStream(string, source))
}

function returnValue<E, T, U>(parser: Parser<E, T>, value: U): Parser<E, U> {
    return stream => (parser(stream), value)
}

class CombinatorDefaults<E, T> implements Combinator<E, T> {
    constructor(public readonly parser: Parser<E, T>) { }
    many0() { return extend(many0(this.parser)) }
    many1() { return extend(many1(this.parser)) }
    many1Fold<S>(init: () => S, folder: (state: S, value: T) => S) { return extend(many1Fold(this.parser, init, folder)) }
    skipMany0() { return extend(skipMany0(this.parser)) }
    skipMany1() { return extend(skipMany1(this.parser)) }
    sepBy1<U>(sep: Combinator<E, U>) { return extend(sepBy1(this.parser, sep.parser)) }
    sepBy0<U>(sep: Combinator<E, U>) { return extend(sepBy0(this.parser, sep.parser)) }
    skipSepBy1<U>(sep: Combinator<E, U>) { return extend(skipSepBy1(this.parser, sep.parser)) }
    left<T2>(parser2: Combinator<E, T2>) { return extend(left(this.parser, parser2.parser)) }
    right<T2>(parser2: Combinator<E, T2>) { return extend(right(this.parser, parser2.parser)) }
    notFollowedBy() { return extend(notFollowedBy(this.parser)) }
    map<U>(mapping: (x: T) => U) { return extend(map(this.parser, mapping)) }
    opt() { return extend(opt(this.parser)) }

    pipe<U>(...parserOrMappings: (Combinator<E, T> | ((...args: T[]) => U))[]): Combinator<E, U> {
        const mapping = parserOrMappings.pop() as (...args: T[]) => U
        const parsers = (parserOrMappings as Combinator<E, T>[]).map(c => c.parser)
        parsers.unshift(this.parser)
        return extend(pipeAny<E, T, U>(parsers, mapping))
    }
    
    tuple(): Combinator<E, [T]>
    tuple<T2>(parser2: Combinator<E, T2>): Combinator<E, [T, T2]>
    tuple<T2, T3>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>): Combinator<E, [T, T2, T3]>
    tuple<T2, T3, T4>(parser2: Combinator<E, T2>, parser3: Combinator<E, T3>, parser4: Combinator<E, T4>): Combinator<E, [T, T2, T3, T4]>
    tuple(...parsers: Combinator<E, T>[]): Combinator<E, T[]> {
        const ps = parsers.map(c => c.parser)
        ps.unshift(this.parser)
        return extend(tupleAny<E, T>(ps))
    }

    return<U>(value: U) { return extend(returnValue(this.parser, value)) }

    skipped(this: Combinator<CodePoint, T>) { return extend(skipped(this.parser)) }
    manyChars0(this: Combinator<E, CodePoint>) { return extend(manyChars0(this.parser)) }

    run(source: ReadonlyArray<E>) {
        return run(this.parser, source)
    }
    runString(this: Combinator<CodePoint, T>, string: string, source: string | null = null) {
        return runString(this.parser, string, source)
    }
}

export function extend<E, T>(parser: Parser<E, T>): Combinator<E, T> {
    return new CombinatorDefaults(parser)
}

// combinators

export function skipSeq<E = DefaultElement>(...parsers: Combinator<E, mixed>[]) {
    return extend<E, null>(stream => {
        for (let i = 0; i < parsers.length; i++) {
            parsers[i].parser(stream)
        }
        return null
    })
}

export function choice<T, E = DefaultElement>(...parsers: Combinator<E, T>[]) {
    return extend<E, T>(choiceParsers(parsers.map(p => p.parser)))
}

// char parsers

export function char(target: CodePoint) {
    const errorMessage = String.fromCodePoint(target)
    return extend<CodePoint, CodePoint>(stream => {
        const { length, position } = stream
        if (length <= position) { return error(errorMessage, stream) }

        const char = stream.buffer[position]
        if (char !== target) { return error(errorMessage, stream) }

        stream.advance(char)
        return char
    })
}

export function choose<T extends {} | null, E = DefaultElement>(chooser: (char: E) => Optional<T>, label = "choose") {
    return extend<E, T>(stream => {
        const { length, position } = stream
        if (length <= position) { return error(label, stream) }

        const char = stream.buffer[position]
        const result = chooser(char)
        if (result === void 0) { return error(label, stream) }

        stream.advance(char)
        return result
    })
}

export function satisfy<E = DefaultElement>(predicate: (char: E) => boolean, label: string) {
    return extend<E, E>(stream => {
        const { length, position } = stream
        if (length <= position) { return error(label, stream) }

        const char = stream.buffer[position]
        if (predicate(char)) {
            stream.advance(char)
            return char
        }

        return error(label, stream)
    })
}

export const anyChar = extend<mixed, mixed>(stream => {
    const { length, position } = stream
    if (length <= position) { return error("anyChar", stream) }

    const char = stream.buffer[position]
    stream.advance(char)
    return char
})

export function string(string: string) {
    const target = ex.String.codePoints(string)
    const targetLength = target.length
    return extend<CodePoint, string>(stream => {
        const { length, position } = stream
        if (length <= position + targetLength - 1) { return error(string, stream) }

        for (let i = 0; i < targetLength; i++) {
            if (stream.buffer[position + i] !== target[i]) { return error(string, stream) }
        }
        for (let i = 0; i < targetLength; i++) {
            stream.advance(target[i])
        }
        return string
    })
}

function regexpEscape(s: string ) { return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&') }

export function noneof(chars: string) {
    const target = ex.String.codePoints(chars)
    return satisfy((char: CodePoint) => !target.includes(char), `[^${regexpEscape(chars)}]`)
}

export function anyof(chars: string) {
    const target = ex.String.codePoints(chars)
    return satisfy((char: CodePoint) => target.includes(char), `[${regexpEscape(chars)}]`)
}
export const eos = extend(stream => {
    if (stream.length <= stream.position) { return null }
    return error("eos", stream)
})

function isAsciiLetter(char: CodePoint) {
    return char <= CodePoint.AsciiMax && (char |= CodePoint[" "], CodePoint.a <= char && char <= CodePoint.z)
}
function isAsciiAToF(char: CodePoint) {
    return char <= CodePoint.AsciiMax && (char |= CodePoint[" "], CodePoint.a <= char && char <= CodePoint.f)
}
function isAsciiDigit(char: CodePoint) {
    return CodePoint._0 <= char && char <= CodePoint._9
}
export const asciiLetter = satisfy<CodePoint>(isAsciiLetter, "a-zA-Z")
export const asciiDigit = satisfy<CodePoint>(isAsciiDigit, "0-9")
export const asciiHex = satisfy<CodePoint>(char => isAsciiDigit(char) || isAsciiAToF(char), "0-9a-fA-F")

export interface CombinatorCell<E, T> {
    parser: Combinator<E, T>
}
function parseToSnapshot<E, T>(parser: Parser<E, T>, stream: Stream<E>): Snapshot<T> {
    try {
        const value = parser(stream)
        return {
            tag: SnapshotTag.Ok,
            value,

            position: stream.position,
            line: stream.line,
            column: stream.column,

            errorPosition: stream.errorPosition,
            errorLine: stream.errorLine,
            errorColumn: stream.errorColumn,
        }
    }
    catch (error) {
        return {
            tag: SnapshotTag.Error,
            message: String(error),

            position: stream.position,
            line: stream.line,
            column: stream.column,

            errorPosition: stream.errorPosition,
            errorLine: stream.errorLine,
            errorColumn: stream.errorColumn,
        }
    }
}
function returnSnapshot<T>(snapshot: Snapshot<T>) {
    if (snapshot.tag === SnapshotTag.Ok) { return snapshot.value }
    throw snapshot.message
}
function playSnapshot<E, T>(snapshot: Snapshot<T>, stream: Stream<E>) {
    stream.position = snapshot.position
    stream.line = snapshot.line
    stream.column = snapshot.column

    stream.errorPosition = snapshot.errorPosition
    stream.errorLine = snapshot.errorLine
    stream.errorColumn = snapshot.errorColumn

    return returnSnapshot(snapshot)
}

export function createParserForwardedToRef<T, E = DefaultElement>(key: string): [Combinator<E, T>, CombinatorCell<E, T>] {
    const cell: CombinatorCell<E, T> = {
        parser: extend(() => { throw "createParserForwardedToRef: not initialized parser" })
    }
    const parser = extend<E, T>(stream => {
        const { cache, position } = stream
        const memo = (cache[key] || (cache[key] = new Map<number, Snapshot<T>>())) as Memo<T>
        const snapshot = memo.get(position)
        if (snapshot === void 0) {
            const snapshot = parseToSnapshot(cell.parser.parser, stream)
            memo.set(position, snapshot)
            return returnSnapshot(snapshot)
        }
        else {
            return playSnapshot(snapshot, stream)
        }
    })
    return [parser, cell]
}