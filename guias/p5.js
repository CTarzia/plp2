// ej1
let o = {a : 1,
    b : function (n){return  this.a + n}};

// let cli = {r : 1, 
//     i : (Math.sqrt(-1)),
//     sumar : function (c){return }};

//ej 2
let t = {ite : function (a,b){return a},
        mostrar : "verdadero",
        and : function (a){return a}
    }

let f = {
    ite : function (a,b){return b}, 
    mostrar : "falso",
    and : function (a){return this}
}

t.not = f
f.not = t

//ej3
let cero = {
    isZero : function() {return true},
    succ : function() {return natural(1)},
    toNumber : function() {return 0},
    for : function(o) {o.eval(this)}
}

function natural(i) {
    if (i == 0) {
        let obj = cero
        return obj
    } else {
        let obj = {
            isZero : function() {return false},
            succ : function() {return natural (i+1)},
            pred : function(){return natural (i-1)},
            toNumber : function(){return i},
            for : function(o) {(natural (i-1)).for(o); o.eval(this)}
        }
        return obj
    }
}
let fun = {eval : function(i) {console.log(i.toNumber())}}
//ej 7
function C1() {}
C1.prototype.g = "hola"
function C2() {}
C2.prototype.g = "mundo"
let a = new C1()
// C1.prototype = C2.prototype // hola, mundo
// C1.prototype.g = C2.prototype.g // mundo, mundo
Object.setPrototypeOf(C1,C2) //hola, hola //????
let b = new C1()
console.log(a.g)
console.log(b.g)

// console.log(cero.succ().succ().for(fun))