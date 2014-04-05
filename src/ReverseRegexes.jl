module ReverseRegexes

export @rr_str, @rr_mstr

import Base: print, reverse, rsearch

type ReverseRegex
    rx::Regex
end

type RTok{T}
    args::Vector{Any}
end

RTok(tag::Symbol, args...) = RTok{tag}({args...})
tag{T}(::RTok{T}) = T
hastag{T}(::RTok{T}, tag::Symbol) = T == tag
hastag{T}(::RTok{T}, tag::Vector) = T in tag

containstag(tok::RTok, tag) = any(x->hastag(x, tag), tok.args)

const invalid_tags = [:(:any0), :(:more0), :(:opt0), :(:alt0), :(:curly0)]

gtag = ['(' => :group, '[' => :class]

immutable RState{L}
    i::Int
    tagstack::NTuple{L,Symbol}
    pclass::Symbol # POSIX-class state (:none -> :brackL -> :colonL -> :colonR)
end
RState(pattern::String) = RState{1}(start(pattern), (:root,), :none)
RState{L}(s::RState{L}, i::Int; pclass=s.pclass) = RState{L}(i, s.tagstack, pclass)
RState{L}(s::RState{L}, i::Int, tag::Symbol) = RState{L+1}(i, tuple(s.tagstack..., tag), :none)

pop(s::RState{0}, i::Int) = error("game over")
pop{L}(s::RState{L}, i::Int) = RState{L-1}(i, tuple(s.tagstack[1:end-1]...), :none)

level{L}(::RState{L}) = L
closes(c, s::RState{0}) = error("game over")
closes(c, s::RState{1}) = error("unmatched $c")
function closes(c, s::RState)
    if c == ')'
        s.tagstack[end] == :group && return true
        s.tagstack[end] == :class && return false
        error("unmatched $c")
    elseif c == ']'
        s.tagstack[end] == :class && return true
        error("unmatched $c")
    else
        error("bug")
    end
end

inclass(s::RState{0}) = error("game over")
inclass(s::RState{1}) = false
inclass(s::RState) = s.tagstack[end] == :class

function advance_pclass(pclass::Symbol, c::Char)
    if pclass == :none
        c == '[' && return :brackL
        return :none
    elseif pclass == :brackL
        c == ':' && return :colonL
        return :none
    elseif pclass == :colonL
        c == ':' && return :colonR
        return :colonL
    elseif pclass == :colonR
        c != ']' && return :none
        return pclass
    end
end
closes_pclass(pclass::Symbol) = pclass == :colonR

checkclosed(s::RState{0}) = error("game over")
checkclosed(s::RState{1}) = true
checkclosed{L}(s::RState{L}) = error("premature end of input")

function parse_regex_token(pattern::String, state::RState)
    i = state.i
    if done(pattern, i)
        checkclosed(state)
        return RTok(:done), RState(-1, (), :none)
        irintln("QUI1")
    end
    c, i = next(pattern, i)
    if c == '\\'
        done(pattern, i) && return RTok(:char, "\\"), RState(state, i)
        c, i = next(pattern, i)
        return RTok(:char, string("\\", c)), RState(state, i)
    elseif c == ']' && closes_pclass(state.pclass)
        return RTok(:char, string(c)), RState(state, i, pclass=:none)
    elseif c in [')', ']']
        closes(c, state) && return RTok(:pop), pop(state, i)
        return RTok(:char, string(c)), RState(state, i)
    elseif inclass(state)
        pclass = advance_pclass(state.pclass, c)
        return RTok(:char, string(c)), RState(state, i, pclass=pclass)
    elseif c == '|'
        return RTok(:alt0), RState(state, i)
    elseif c == '{'
        return RTok(:curlyL), RState(state, i)
    elseif c == '}'
        return RTok(:curlyR), RState(state, i)
    elseif c in ['(', '[']
        t = gtag[c]
        tok = RTok(t)
        l0 = level(state)
        state = RState(state, i, t)
        while true
            subex, state = parse_regex_token(pattern, state)
            level(state) <= l0 && break
            push!(tok.args, subex)
        end
        return tok, state
    elseif c == '*'
        return RTok(:any0), RState(state, i)
    elseif c == '+'
        return RTok(:more0), RState(state, i)
    elseif c == '?'
        return RTok(:opt0), RState(state, i)
    elseif c == '^'
        return RTok(:sol), RState(state, i)
    elseif c == '$'
        return RTok(:eol), RState(state, i)
    else
        return RTok(:char, string(c)), RState(state, i)
    end
end

pptag{T}(regex::RTok{T}) = symbol(string(T)[1:end-1]) # :any0 => :any, :more0 => :more etc.
const _tagsymbol = [:any => "*", :more => "+", :opt => "?", :curly => "{...}"]
tagsymbol(tag::Symbol) = _tagsymbol[tag]
tagsymbol{T}(regex::RTok{T}) = _tagsymbol[T]

function staged_is_valid(staged::Vector{Any})
    isempty(staged) && return false
    hasdigits = false
    hascomma = false
    for tok in staged
        hastag(tok, :char) || return false
        length(tok.args[1]) == 1 || return false
        c = tok.args[1][1]
        if isdigit(c)
            hasdigits = true
        elseif c == ','
            hasdigits || return false
            hascomma && return false
            hascomma = true
        else
            return false
        end
    end
    return true
end

for T in [:(:char), :(:sol), :(:eol)]
    @eval postparse_regex(regex::RTok{$T}) = regex
end
function postparse_regex(regex::RTok)
    if containstag(regex, :alt0)
        groups = {{}}
        for x in regex.args
            if hastag(x, :alt0)
                push!(groups, {})
                continue
            end
            push!(groups[end], x)
        end
        pp = RTok(tag(regex), RTok(:alt, [postparse_regex(RTok(:altgroup, g...)) for g in groups]...))
        return pp
    end
    staged = {}
    iR = 0
    i = length(regex.args)
    while i >= 1
        if hastag(regex.args[i], :curlyR)
            iR > 0 && (staged = {})
            iR = i
        elseif hastag(regex.args[i], :curlyL)
            if iR > 0 && staged_is_valid(staged)
                splice!(regex.args, i:iR, [RTok(:curly0, staged...)])
            end
            iR = 0
            staged = {}
        elseif iR > 0
            unshift!(staged, regex.args[i])
        end
        i -= 1
    end
    i = length(regex.args)
    while i >= 1
        if hastag(regex.args[i], [:any0, :more0, :opt0])
            t = pptag(regex.args[i])
            i > 1 || error("'" * tagsymbol(t) * "' at the beginning of an atom")
            splice!(regex.args, i-1:i, [postparse_regex(RTok(t, regex.args[i-1]))])
            i -= 1
        elseif hastag(regex.args[i], :curly0)
            t = pptag(regex.args[i])
            i > 1 || error("'" * tagsymbol(t) * "' at the beginning of an atom")
            curlytarget = RTok(:curlytarget, postparse_regex(regex.args[i-1]))
            curlycontent = RTok(:curlycontent, map(postparse_regex, regex.args[i].args)...)
            splice!(regex.args, i-1:i, [RTok(t, curlytarget, curlycontent)])
            i -= 1
        else
            regex.args[i] = postparse_regex(regex.args[i])
        end
        i -= 1
    end
    return regex
end

function regex(pattern::String)
    ex = RTok(:regex_tree)
    state = RState(pattern)
    while true
        subex, state = parse_regex_token(pattern, state)
        hastag(subex, :done) && break
        push!(ex.args, subex)
    end
    ex = postparse_regex(ex)
end

reverse(regex::RTok) = regex
reverse(regex::RTok{:sol}) = RTok(:eol)
reverse(regex::RTok{:eol}) = RTok(:sol)
for T in [:(:regex_tree), :(:altgroup), :(:group)]
    @eval reverse(regex::RTok{$T}) = RTok($T, map(reverse, reverse(regex.args))...)
end
reverse(regex::RTok{:alt}) = RTok(:alt, map(reverse, regex.args)...)
for T in invalid_tags
    @eval reverse(regex::RTok{$T}) = error("invalid tag: " * string($T))
end

print(io::IO, regex::RTok{:regex_tree}) = map(ex->print(io, ex), regex.args)
print(io::IO, regex::RTok{:altgroup}) = map(ex->print(io, ex), regex.args)
print(io::IO, regex::RTok{:char}) = print(io, regex.args[1])
for (T,pre,post) in [(:(:any), "", '*'),
                     (:(:more), "", '+'),
                     (:(:opt), "", '?'),
                     (:(:class), '[', ']'),
                     (:(:group), '(', ')')]
    @eval function print(io::IO, regex::RTok{$T})
        print(io, $pre)
        map(a->print(io, a), regex.args)
        print(io, $post)
    end
end
function print(io::IO, regex::RTok{:curly})
    @assert hastag(regex.args[1], :curlytarget)
    @assert hastag(regex.args[2], :curlycontent)
    map(a->print(io, a), regex.args[1].args)
    print(io, '{')
    map(a->print(io, a), regex.args[2].args)
    print(io, '}')
end
function print(io::IO, regex::RTok{:alt})
    for i = 1:(length(regex.args)-1)
        print(io, regex.args[i])
        print(io, '|')
    end
    print(io, regex.args[end])
end
print(io::IO, regex::RTok{:sol}) = print(io, '^')
print(io::IO, regex::RTok{:eol}) = print(io, '$')
print(io::IO, regex::RTok{:curlyL}) = print(io, '{')
print(io::IO, regex::RTok{:curlyR}) = print(io, '}')
for T in invalid_tags
    @eval print(io::IO, regex::RTok{$T}) = error("invalid tag: " * string($T))
end

type ReverseRegex
    rx::Regex
    ReverseRegex(pattern::String) = new(Regex(string(reverse(regex(pattern)))))
    ReverseRegex(pattern::String, flags...) = error("Flags not suported by ReverseRegex")
end

macro rr_str(pattern, flags...) ReverseRegex(pattern, flags...) end
macro rr_mstr(pattern, flags...) ReverseRegex(pattern, flags...) end

function rsearch(str::Union(ByteString,SubString), re::ReverseRegex, idx::Integer)
    start(str) <= idx <= nextind(str,endof(str)) || throw(BoundsError())
    str = str[1:idx]
    rstr = reverse(str)
    r = search(rstr, re.rx)
    isempty(r) && return 0:-1
    convidx(i) = chr2ind(str, length(str)-ind2chr(rstr, i)+1)
    return convidx(last(r)):convidx(first(r))
end
rsearch(str::String, re::Regex, idx::Integer = 0) =
    error("reverse regex search is only available with ReverseRegex objects")
rsearch(s::String, r::ReverseRegex, idx::Integer) =
    error("regex search is only available for bytestrings; use bytestring(s) to convert")
rsearch(s::String, r::ReverseRegex) = rsearch(s, r, endof(s))

end # module
