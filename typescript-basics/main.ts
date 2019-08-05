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
    const firstName = "Lana"; // tslint:disable-line:no-shadowed-variable
    // console.log(firstName); // Lana
}
// console.log(firstName); // Vlad

// const has lexical (block) scope and only forbids assignment
// The variable is still mutable
// Allways start with const and use let only if reqiured
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

// Enumeration (open ended)
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
console.log(number2);
