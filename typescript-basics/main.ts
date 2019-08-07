// Type inference
const radius = 4;
const area = Math.PI * radius ** 2;
// console.log(area.toFixed(3));

// Lexical (block) scope
let globalScope = 1;
{
    const blockScope = 2;
    globalScope = 10;
    // console.log(globalScope, blockScope);
}
const firstName = "Vlad";
{
    const firstName = "Lana";
    // console.log(firstName); // Lana
}
// console.log(firstName); // Vlad

// const has lexical (block) scope and only forbids assignment
// The variable is still mutable
// Allways start with const and use let only if reassignment reqiured
// Avoid using function scope and variable hoisting with var
const digits = [1, 2, 3];
digits.push(4, 5, 6);
// console.log(digits);

// Dynamic variable (no type checking at all)
let dynamicVariable: any;
dynamicVariable = "Vlad";
// console.log(dynamicVariable);

// Array type
// const names: Array<String> = ["Vlad", "Lana"];
const names: string[] = ["Vlad", "Lana"];
// console.log(names)

// Function type annotation
let greet: (name: string) => string;
greet = (name) => `Hello ${name}`;
const greeting = greet("Lana");
// console.log(greeting);

// Object type annotation
let person: {id: number; name: string};
person = {id: 1, name: "Vlad"};
// console.log(person);

// Object type alias
// tslint:disable-next-line:interface-over-type-literal
type PersonType = {id: number; name: string};
const person2: PersonType = {id: 1, name: "Lana"};
// console.log(person2);

// Object interface can be used in implements and extends
// Interface can accept type parameters (generics)
interface IPerson {
    id: number;
    name: string;
}
const person3: IPerson = {id: 1, name: "Vlad"};
// console.log(person3);

// Enumeration is open ended (all declaration under commot root are merged togather)
enum VehicleType {
    Car,
    Van,
    Bus,
    Lorry,
}
const enumIndex = VehicleType.Lorry;
const enumName = VehicleType[enumIndex];
// console.log(enumIndex, enumName);

// Union type
type NumberOrBoolean = number | boolean;
let unionValue: NumberOrBoolean;
unionValue = 1;
// console.log(unionValue);
unionValue = true;
// console.log(unionValue);

// Literal type
type ProgrammingLanguage = "JavaScript" | "TypeScript";
let programmingLanguage: ProgrammingLanguage;
programmingLanguage = "TypeScript";
// console.log(programmingLanguage);

// Intersection type
interface ISkier {
    slide(): void;
}
interface IShooter {
    shoot(): void;
}
type Biathelete = ISkier & IShooter;

// Tuple type
let tuple: [number, string];
tuple = [1, "Lana"];
// console.log(tuple)

// Dictionary type
interface IPersonDictionary {
    [index: string]: IPerson;
}
const personDictionary: IPersonDictionary = {};
/* tslint:disable:no-string-literal */
personDictionary["Vlad"] = {id: 1, name: "Vlad"};
personDictionary["Lana"] = {id: 2, name: "Lana"};
// console.log(personDictionary);
// console.log(personDictionary["Vlad"].id);
/* tslint:enable:no-string-literal */

// Mapped type (keyof)
interface IOptions {
    material: string;
    backlight: boolean;
}
type ReadOnly<T> = {readonly [k in keyof T]: T[k]};
type Optional<T> = {[k in keyof T]?: T[k]};
type Nullable<T> = {[k in keyof T]: T[k] | null};
type ReadOnlyOptions = ReadOnly<IOptions>;
type OptionalOptions = Optional<IOptions>;
type NullableOptions = Nullable<IOptions>;
const options: ReadOnlyOptions = {material: "wood", backlight: false};
// console.log(options);
const options2: OptionalOptions = {};
// console.log(options2);
const options3: NullableOptions = {material: null, backlight: null};
// console.log(options3);

// Type assertion
const name2: string = "1";
// const number2: number = <number>(<any>name2);
const number2: number = (name2 as any) as number;
// console.log(number2);

// Increment and decrement operators
let loopCounter = 0;
do {
    // console.log("do while", loopCounter);
} while (++loopCounter < 4);

// while vs do while
loopCounter = -1;
while (++loopCounter < 4) {
    // console.log("while", loopCounter);
}

// Convert any type to number
const number3 = +"1234";
// console.log(number3);
// Convert any type to boolean
const boolean2 = !!number3;
// console.log(boolean2);

// && check and do idiom
const doIt = false;
// tslint:disable-next-line:no-unused-expression
doIt && console.log("do it");
// || coalesce idiom
let value: string;
value = value || "default";
// console.log(value);

// Array destructuring: skip, rest, default
const array2 = [1, 2, 3, 4, 5, 6];
let [a, b, , ...rest] = array2;
// console.log(a, b, rest);
// console.log(a, b, ...rest);
// Swap variables without a temporary variable
[a, b] = [b, a];
// console.log(a, b);
const array3 = [1, 2];
let [a2, b2, c2 = 0] = array3;
// console.log(a2, b2, c2);

// Object desctructuring: rest
const object2 = {anId: 1, aName: "Vlad", aPosition: "architect"};
// unpack into new variables
const {anId: myId, aName: myName} = object2;
// console.log(myId, myName);
// auto-unpack
const {anId, aName} = object2;
// console.log(anId, aName);
const {aPosition: position, ...aRest} = object2;
// console.log(position, aRest);

// Tuple desctructuring
function returnTuple() {
    return ["Vlad", "Lana"];
}
const [result1, result2] = returnTuple();
// console.log(result1, result2);

// Array spread opertor
const array4 = [1, 2, 3];
const array5 = [10, 20, 30];
const spreadArray = [...array4, ...array5];
// console.log(spreadArray);

// Object spread operator
const object3 = {firstName: "Vlad"};
const object4 = {lastName: "Prokopyuk"};
const spreadObject = {...object3, ...object4};
// console.log(spreadObject);

// Function optional parameters
function fun(x: number, y?: number) {
    return x + (y || 0);
}
let res = fun(1, 2);
// console.log(res);
res = fun(10);
// console.log(res);

// Function default parameters
function fun2(x: number, y: number = 0) {
    return x + y;
}
res = fun(1, 2);
// console.log(res);
res = fun(10);
// console.log(res);

// Function rest parameters
function sum(...elements: number[]): number {
    let total = 0;
    for (const element of elements) {
        total += element;
    }
    return total;
}
res = sum(1, 2, 3, 4, 5, 6, 7);
// console.log(res);

// Arrow function
const addNumbers = (...numbers: number[]): number => {
    const aSum = numbers.reduce((x, y) => x + y, 0);
    return aSum;
};
res = addNumbers(1, 2, 3, 4, 5, 6, 7);
// console.log(res);

// Arrow function preserves the lexical scope of the this keyword
const lostScope = {
    message: "Property from lexical scope",
    // Short syntax for method definitions on object initializer
    notify() {
        // Lexical scope is lost
        setTimeout(function() {
            console.log(this.message);
        }, 1);
    },
};
// lostScope.notify();
const preservedScope = {
    message: "Property from lexical scope",
    // Short syntax for method definitions on object initializer
    notify() {
        // Lexical scope is preserved in arrow function
        setTimeout(() => console.log(this.message), 1);
    },
};
// preservedScope.notify();

// Use arrow function for funciton currying
const multiply = (a3: number) => (b3: number) => a3 * b3;
res = multiply(4)(5);
// console.log(res);
const multiply10 = multiply(10);
res = multiply10(4);
// console.log(res);

// Interface can be used as a) an abstract type to beimplemented by a concrete class
// b) define a data structure c) define a software contracts for third-party libraries
// Interface defines properties, methods, constructors, indexers
// Interface does not generate any JavaScript code (type erasure)
// Interface is open ended (all declaration under commot root are merged togather)
// Interfade can define function/object hybrid type
interface IFunction {
    (message: string): void; // tslint:disable-line:callable-types
}
const fun3: IFunction = (message: string) => console.log(message);
// fun3("Function interface");

// Class access modifies are not controlled at runtime, but enforced at compile time
interface IPlayable {
    play(): void;
}
class Song implements IPlayable {
    constructor(private title: string, private artist: string) {}

    play() {
        console.log(`${this.title} by ${this.artist}`);
    }
}

class Jukebox {
    constructor(private songs: IPlayable[]) {}

    play() {
        const song = this.getRandomSong();
        song.play();
    }

    private getRandomSong() {
        const index = Math.floor(this.songs.length * Math.random());
        return this.songs[index];
    }
}

const songs = [
    new Song("Title 1", "Artist 1"),
    new Song("Title 2", "Artist 2"),
    new Song("Title 3", "Artist 3"),
];
const jukebox = new Jukebox(songs);
// jukebox.play();

// Class property getter and setter
class FullName {
    constructor(private firstName: string, private lastName: string) {}

    get fullName() {
        return `${this.firstName} ${this.lastName}`;
    }

    set fullName(fullName: string) {
        const match = fullName.match(/^(\w+) (\w+)$/);
        if (match) {
            this.firstName = match[1];
            this.lastName = match[2];
        } else {
            throw new Error(`Invalid full name ${fullName}`);
        }
    }
}
const person4 = new FullName("Volodymyr", "Prokopyuk");
// console.log(person4.fullName);
person4.fullName = "Vlad Prokopyuk";
// console.log(person4.fullName);

// Promise represents the eventual completion or failure of an async operation
// async operation returns a Promise to supply the result at some point in the feature
// Promise states: pending > settled (resolved or rejected)
// new Promise((resolve, reject) => {
//     executor is called immediately by new Promise()
//     resolve and reject callbacks are provided by the JavaScript engine
//     only one of resolve or reject must be called only once, further calls are ignored
//     async operation
// })
// Promise.prototype.then(onResolve, onReject) > returns a Promise (Promise chain)
// Promise.prototype.catch(onReject) = .then(null, onReject)
// When a Promise rejects the control jumps to the closes rejection handler
// Promise.prototype.finally(onSettle) = .then(onSettle, onSettle)
// .finally() is not meant to process a Promise result
// .finally() handler passes through results and errors to the next hendler
// Promise.resolve(), Promise.reject()
// Promise.all(iterable) > all resolved or first rejected
// Promise.race(iterable) > first resolved or rejected
// Promise.allSettled(iterable) > all settled (resolved or rejected)
// Promise handling is always async and all Promise handles pass through microtask queue

// new Promise((resolve, reject) => {
//     resolve("OK");
// })
//     .finally(() => console.log("clean up"))
//     .then((result) => console.log(result));

// new Promise((resolve, reject) => {
//     reject("OH");
// })
//     .finally(() => console.log("clean up"))
//     .catch((error) => console.error(error));

// const resolved = new Promise((resolve, reject) => {
//     setTimeout(() => resolve("OK"), 1000);
// });
// const rejected = new Promise((resolve, reject) => {
//     setTimeout(() => reject("OH"), 500);
// });
// Promise.race([resolved, rejected])
//     .then(console.log)
//     .catch(console.error);

// async/await: replaces .then() with await and .catch() with try { await } catch()
// write completely sync-looking code while performing async operations
// The functionality of async/await can be achieved with Promises + generators (yield)
function async1Secs() {
    return new Promise((resolve) => {
        setTimeout(() => resolve("RESOLVED: 1 sec"), 1000);
    });
}
function async2Secs() {
    return new Promise((resolve) => {
        setTimeout(() => resolve("RESOLVED: 2 sec"), 2000);
    });
}
// async function always returns a Promise
// non-Promise result is automatically wrapped in a Promise
// await Promise waits for a Promise to be settled
// await Promsie can be used only inside async function
// await does not work in top-level code > wrap the code in IIFE: (async() => {await})
// if the Promise resolves, then await Promise returns the resutl
// if the Promise rejects, then await Promise throws an error
// await Promise.reject(new Error("oh")) === throw new Error("oh")
// Use try { await } catch
// asyncSequential finishes after 2 + 1 = 3 secs
async function asyncSequential() {
    // Sequential calls to async functions
    const result2 = await async2Secs();
    console.log(result2);
    const result1 = await async1Secs();
    console.log(result1);
}
// asyncSequential();
// asyncConcurrent finishes after 2 secs
async function asyncConcurrent() {
    // Concurrent calls to async functions
    const result2 = async2Secs();
    const result1 = async1Secs();
    console.log(await result2);
    console.log(await result1);
}
// asyncConcurrent();
// await for multiple Promises with Promise.all()
async function concurrentPromises() {
    const [result1, result2] = await Promise.all([async1Secs(), async2Secs()]);
    console.log(result1);
    console.log(result2);
}
// concurrentPromises();

// async/await error handling
function resolveOrReject() {
    const success = Math.round(Math.random());
    return new Promise((resolve, reject) => {
        success ? resolve("OK") : reject("OH");
    });
}
async function showAsyncResult() {
    try {
        const result = await resolveOrReject();
        console.log(result);
    } catch (error) {
        console.log(error);
    }
}
// for (let i = 0; i < 5; ++i) {
//     showAsyncResult();
// }
