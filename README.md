# Learning programming examples

## Python basics

## Bash basics

## PostgreSQL basics

### Реляционная модель

- Данние воспринимаются пользователем как таблицы
- Способность поддерживать связи между элементами данных
- Каскадное обновление или удаление данных (автоматическое и декларативное)
- До реляционная эпоха: файли, записи, поля
- Реляционная эпоха: таблица, строка, столбец (атомарное значение)
- Формальная теория: отношение, кортеж, атрибут (теория множеств и исчисление
  предикатов)
    - Отношение: заголовок и тело
        - Степень отношения - количество атрибутов
        - Кардинальное число - количество кортежей
- Ключи - используются для идентификации строк в таблицах и для связи таблиц между собой
    - Потенциальный ключ - неизбыточная комбинация атрибутов таблицы, позволяющая
      уникальным образом идентифицыровать строки в ней
    - Первычный ключ - один из потенциальных ключей (остальные потенциальные ключи
      алтернативные ключи)
        - Первычный ключ может быть естественным или суррогатным
        - Суррогатных ключ - позволяет сократить число атрибутов во внешних ключах до
          одного
    - Внешний ключ - ссылается на потенцыальный ключ в ссылочной таблице (ссылочная
      целостность)
        - Внешний ключ служит для связи таблиц между собой
        - Внешний ключ не обязан быть уникальным
        - Внешний ключ обеспечивает каскадное обновление и удаление данных
- Индекс - ускоряет доступ к строкам таблицы и предотвращает дублирование значений
  ключевых атрибутов
    - Для первичного ключа индекс создается автоматически
- Транзакция - набор операций над базой данных, рассматриваемых как единая и неделимая
  единица работы, выполняемая полностью или не выполняемая вовсе, если произошел сбой
  (обеспечение согласованности данных)

### Язык SQL

- SQL является основным и единым языком доступа к RDBMS
    - Data Definition Language (DDL): CREATE, ALTER, DROP
    - Data Manupulaiton Language (DML): INSERT, UPDATE, DELETE, SELECT
    - Transaction Control Language (DCL): BEGIN, COMMIT, ROLLBACK
    - Data Control Language (DCL): GRANT, REVOKE

### Типы данных

- Числовые типы
    - Челочисленные типы: smallint, integer, bigint
    - Числа фиксированной точности: numeric(scale, precision) (псевдоним decimal)
        - SELECT 0.1::numeric * 10 = 1.0::numeric > True
    - Числа с плавающей точкой: real, double precision (Inf, -Inf, NaN)
        - float(p) може быть или real или double precision в зависимости то (p)
          (стандарт SQL)
        - SELECT 0.1::real * 10 = 1.0::real > False
    - Последовательные типы: smallserial, serial, bigserial (уникальные значения
      суррогатного первичного ключа)
        - pk serial = CREATE SEQUENCE sq, pk integer NOT NULL DEFAULT nextval(sq), ALTER
          SEQUENCE sq OWNED BY pk
- Строковые типы
    - character(n) - ограниченная строка, дополняется пробелами (псевдоним char(n))
    - character varying(n) - ограниченная строка, не дополняется пробелами (псевдоним
      varchar(n))
    - text - неограниченная строка
- Типы дати и времени
    - date - current_date
    - time - current_time(p)
    - timestamp - current_timestamp(p), timestamp '2019-05-31 12:34:56'
    - timestamptz - current_timestamp at time zone 'UTC'
    - interval - '1 year 2 month [ago]'::interval
- Логический тип
    - boolean - TRUE, FALSE, NULL (трехзначная логика)
- Массивы
    - integer[] - '{1, 2, 3, 4}'::integer[], '{"alpha", "beta", "gamma"}'::text[],
      arr[1], arr[1:2]
- Типы JSON
    - json - JSON сохраняется в строковом виде, каждый раз нужно делать разбор при
      доступе
    - jsonb - JSON сохраняется в двоичном виде, быстрый доступ (рекомендуемый тип)
    - '{"name": "Vlad", "happy": true}'::jsonb, json->'name', json->0
    - SELECT '{"first_name": "Volodymyr"}'::jsonb || '{"last_name": "Prokopyuk"}'::jsonb;
    - SELECT '{"first_name": "Vlad"}'::jsonb || '{"last_name": "Prokopyuk"}'::jsonb -
      'last_name';

### Язык опредения данных

- Значение по умолчанию <атрибут> DEFAULT <выражение> (current_timestamp, current_user)
- Ограничение <атрибут> NOT NULL (CHECK <атрибут> IS NOT NULL)
- CONSTRAINT <имя ограничения>
- Ограничение CHECK <выражение>
- Ограничение UNIQUE <атрибуты> (потенциальный ключ)
    - Для ограничений UNIQUE автоматически создается индекс
- Ограничение PRIMARY KEY <атрибуты> (UNIQUE <атрибуты> NOT NULL) (первычный ключ)
    - Для ограничений PRIMARY KEY автоматически создается индекс
- Ограничение FOREIGN KEY <атрибуты FK> REFERENCES <атрибуты PK> ON {UPDATE | DELETE}
  {CASCADE | RESTRICT (default) | SET NULL | SET DEFAULT} (ссылочная целостность)
  (внешний ключ)
- Коммнтарии COMMENT ON {DATABASE | SHCEMA | TABLE | COLUMN} <table>.<column> IS <comment>;
- CREATE VIEW <view> AS <select>; SELECT * FROM <view>
    - В отличии от таблиц, представления не содержат данных. При каждом обращению к
      представлению данные выбираються из таблиц
    - Представления являються интерфейсом доступа к данным, но сам запрос <select> может
      изменяться. Использование представлений упрощает присладные программы и скривает
      сложноть запросов
    - Представления избавляют от необходимости создавать дополнительные таблицы,
      дублируя данные для разных нужд использования данных
    - Представления являються сохранненными запросами в базе данных
    - Представления являються средством разграничения полномочий доступа к данным
- CREATE MATERIALIZED VIEW <view> AS <select>; REFRESH MATERIALIZED VIEW <view>;
  SELECT * FROM <view>
    - Материализованное представление снижает время выполнения сложных запросов
    - Пример: для формарования отчета требуется длительное врея, а запросы к отчету
      будут неоднократными
- CREATE SCHEMA <schema>; SET search_path = <schemas>
    - Схема это логическая часть базы данных, в которой содержаться объекты бази данних
      (таблицы, представления)
    - Схема образут пространство имен для объектов бази данных

### Язык изменения данных

- INSERT INTO <table> SELECT * FROM <table> [RETUNRNING <expression>]
- INSERT INTO <table> VALUES <vlues> [ON CONFLICT {DO NOTHING | DO UPDATE SET <values>}]
  [RETUNRNING <expression>]
- COPY <table> FROM <file> - массовый ввод данных с файла
- COPY <table> TO <file> - массовый вывод данных в файл
- UPDATE <table> SET <values> [FROM <tables>] WHERE <conditions> [RETUNRNING <expression>]
- DELETE FROM <table> [USING <tables>] WHERE <conditions> [RETUNRNING <expression>]
- TRUNCATE <table>

## Idris basics
