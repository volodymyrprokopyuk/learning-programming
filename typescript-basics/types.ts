// Type defines: meaning of data, set of values, set of operations
// Type ensures data integrity and enforces data access
// Type defines immutability (const), encapsulation (private, scope)
// Type allows for composability, modularity, and documentation

const _findFirstNegativeNumber = (numbers: number[]): number | undefined => {
    for (const num of numbers) {
        if (num < 0) {
            return num;
        }
    }
};

const _findFirstOneCharString = (strings: string[]): string | undefined => {
    for (const str of strings) {
        if (str.length === 1) {
            return str;
        }
    }
};

const findFirst = <T>(items: T[], predicate: (item: T) => boolean): T | undefined => {
    for (const it of items) {
        if (predicate(it)) {
            return it;
        }
    }
};

const findFirstNegativeNumber = (numbers: number[]): number | undefined => {
    return findFirst(numbers, (num) => num < 0);
};

const findFirstOneCharString = (strings: string[]): string | undefined => {
    return findFirst(strings, (str) => str.length === 1);
};

// const numbers = [1, 2, -4];
// const negativeNumber = findFirstNegativeNumber(numbers);
// console.log(negativeNumber);

// const strings = ["ab", "cd", "e"];
// const oneCharString = findFirstOneCharString(strings);
// console.log(oneCharString);

// Static typing (TypeScript), dynamic/duck typing (JavaScript)
// Strong typing (===) + explicit coersion, weak typing (==) + imlicit coersion

interface Duck {
    quack(): void;
}

const quacker = (duck: Duck) => {
    duck.quack();
};

// quacker({quack: () => console.log("quack")});

// Roadmap
// Primitive types, type composition (structural programming)
// Function types (functional programming)
// Subtyping: inheritance, composition, misins (object-oriented programming)
// Generics: type variables (generic programming)

// never (empty type, no value): throw, while(true): funciton does not return at all

const throwError = (message: string): never => {
    throw new Error(message);
};

const infinitLoop = (): never => {
    while (true) {
        console.log("processing...");
    }
};

// void (unit type, single value): side effect: function does not return any meaningful
// value, but returns

const logMessage = (message: string): void => {
    console.log(message);
};

// boolean (two valuesa): conditional branching and looping, short-circuiting
// number: unsigned integer, signed integer, float-point, overflow, underflow
// string: encoding
// Fixed-size array, linked list, binary tree, hash table

// Tuple: fixed array of different types with positioned access defined inline to group
// data in an ad hoc way

type Point = [number, number];

const distance = (point1: Point, point2: Point) => {
    return Math.sqrt((point1[0] - point2[0]) ** 2 + (point1[1] - point2[1]) ** 2);
};

// Record (data class): set of different types with access by name defined separately
// Use private members + access methods (get, set) to ensure data invariants
// Use immutable memebers + init logic in constructor to ensure data invariants

class Point2 {
    x: number;
    y: number;

    constructor(x: number, y: number) {
        this.x = x;
        this.y = y;
    }
}

const distance2 = (point1: Point2, point2: Point2) => {
    return Math.sqrt((point1.x - point2.x) ** 2 + (point1.y - point2.y) ** 2);
};

// Enumeration: one of explicitly declared values

enum DayOfWeek {
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saterday,
    Sunday,
}

const isWeekend = (day: DayOfWeek) => {
    return [DayOfWeek.Saterday, DayOfWeek.Sunday].some((dow) => dow === day);
};

// DayOfWeek | undefined, DayOfWeek | Error: either valure or error
// Union types
// Visitor pattern implements double dispatch
//     accept(visitor) operation
//     visit(instance) instance

// Algebraic data types
//     Product types: compound types: tuple, record
//     Sum types: either-or types: enum, DayOfWeek | Error

// Primitive obsession antipattern: define dedicated types for values
// Enforce acceptable value range in a constructor

// First-class function + function type () => void
// Lambda function = anonymous function
// Closure

const makeClounter = (start: number = 0) => {
    let counter = start;
    return () => counter++;
};

const counter1 = makeClounter();
const counter2 = makeClounter();
// console.log(counter1(), counter2(), counter2(), counter1());

// Callback, promise, async/await
