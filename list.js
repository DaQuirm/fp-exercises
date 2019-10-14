// Y and foldl using Y
// unlist using f t l and use for show?

const Nil = n => _ => n
const Cons = h => t => _ => fc => fc(h)(t)

const l3 = Cons(2)(Cons(3)(Cons(5)(Nil)))
const l5 = Cons(2)(Cons(3)(Cons(5)(Cons(7)(Cons(11)(Nil)))))

const show = l => l('')(h => t => `${h}${t('')(_ => _ => ` ${show(t)}`)}`)
// console.log(show(l3))

const foldl = f => z => l => l(z)(h => t => foldl(f)(f(z)(h))(t))

const reverse = foldl(acc => x => Cons(x)(acc))(Nil)
// console.log(show(reverse(l3)))

const length = foldl(acc => _ => acc + 1)(0)
// console.log(length(l3))

const foldr = f => z => l => l(z)(h => t => f(h)(foldr(f)(z)(t)))

const clone = foldr(Cons)(Nil)
// console.log(show(clone(l3)))

const filter = p => foldr(x => acc => p(x) ? Cons(x)(acc) : acc)(Nil)
// console.log(show(filter(x => x % 2 === 1)(nat(37))))

const flip = f => x => y => f(y)(x)

const concat = flip(foldr(Cons))
// console.log(show(concat(l3)(l5)))

const map = f => foldr(x => acc => Cons(f(x))(acc))(Nil)
// console.log(show(map(x => x * 2)(l3)))

const Tuple = a => b => fab => fab(a)(b)
Tuple.show = tuple => `(${tuple(a => b => `${a}, ${b}`)})`

const tuples = map(x => Tuple.show(Tuple(x)(x + 1)))(l3)
// console.log(show(tuples))

const drop = n => l =>
    n === 0
        ? l
        : l(Nil)(_ => t => drop(n - 1)(t))
// console.log(show(drop(3)(l5)))

const take = n => l =>
    n === 0
        ? Nil
        : l(Nil)(h => t => Cons(h)(take(n - 1)(t)))
// console.log(show(take(3)(l5)))

const nat = n => {
    const go = m => Cons(m)(m === n ? Nil : go(m + 1))
    return go(1)
}
// console.log(show(nat(11)));

const foldMap = f => foldl(acc => x => concat(f(x))(acc))(Nil)
// console.log(show(foldMap(x => Cons(-x)(Cons(x)(Nil)))(l5)))

const zipWith = f => xs => ys =>
    xs(Nil)(hx => tx => ys(Nil)(hy => ty => Cons(f(hx)(hy))(zip(tx)(ty))))

const zip = zipWith(Tuple)
// const zipped = zip(nat(5))(l5)
// console.log(show(map(Tuple.show)(zipped)))


// const unzip = foldr(t => acc => t(a => b => acc(as => bs => Tuple(Cons(a)(as))(Cons(b)(bs)))))(Tuple(Nil)(Nil))

Tuple.bimap = fl => fr => t => t(a => b => Tuple(fl(a))(fr(b)))
// const mt = Tuple.bimap(x => x * x)(x => -x)(Tuple(5)(3))
// console.log(Tuple.show(mt))

const id = x => x

Tuple.lmap = f => Tuple.bimap(f)(id)
Tuple.rmap = f => Tuple.bimap(id)(f)
Tuple.map = f => Tuple.bimap(f)(f)

const unzip = foldr(t => acc => t(a => b => Tuple.bimap(Cons(a))(Cons(b))(acc)))(Tuple(Nil)(Nil))
// const zipped = zip(nat(5))(l5)
// console.log(Tuple.show(Tuple.map(show)(unzip(zipped))))

const NonEmpty = Tuple

const ne7 = NonEmpty(10)(drop(10)(nat(17)))

NonEmpty.show = t => t(x => xs => `${x}${xs('')(_ => _ => ` ${show(xs)}`)}`)
// console.log(NonEmpty.show(ne7))

NonEmpty.foldr = f => z => l => l(h => t => f(h)(foldr(f)(z)(t)))

NonEmpty.toList = NonEmpty.foldr(Cons)(Nil)
// console.log(show(NonEmpty.toList(ne7)))

NonEmpty.map = f => l => l(h => t => NonEmpty(f(h))(map(f)(t)))
// console.log(NonEmpty.show(NonEmpty.map(x => x * x)(ne7)))

NonEmpty.head = l => l(h => _ => h)
// console.log(NonEmpty.head(ne7))

const Nothing = n => _ => n
const Just = a => _ => j => j(a)

const Maybe = {}
Maybe.show = m => m('Nothing')(a => `Just ${a}`)
Maybe.map = f => m => m(Nothing)(f)

const head = l => l(Nothing)(h => _ => Just(h))
const tail = l => l(Nothing)(_ => t => Just(t))

const nth = n => l => head(drop(n)(l))
// console.log(Maybe.show(nth(22)(nat(20))))
// console.log(Maybe.show(nth(7)(nat(20))))
