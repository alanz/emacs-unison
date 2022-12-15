## Unison Codebase Manager API

Provides operations for querying and manipulating a Unison codebase.

## GET /definitions/terms/by-hash/:hash/summary

### Captures:

- *hash*: A hash reference for a term. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef

### GET Parameters:

- name
     - **Values**: **
     - **Description**: A definition name. See API documentation to determine how it should be qualified.

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace root. If left absent, the most recent root will be used.

- relativeTo
     - **Values**: **
     - **Description**: The namespace relative to which names will be resolved and displayed. If left absent, the root namespace will be used.E.g. base.List

- renderWidth
     - **Values**: *80, 100, 120*
     - **Description**: The preferred maximum line width (in characters) of the source code of definitions to be rendered. If left absent, the render width is assumed to be Width {widthToInt = 80}.


### Response:

- Status code 200
- Headers: [("Cache-Control","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /definitions/types/by-hash/:hash/summary

### Captures:

- *hash*: A hash reference for a type. E.g. @abcdef, #abcdef, @@builtin, ##builtin, abcdef

### GET Parameters:

- name
     - **Values**: **
     - **Description**: A definition name. See API documentation to determine how it should be qualified.

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace root. If left absent, the most recent root will be used.

- relativeTo
     - **Values**: **
     - **Description**: The namespace relative to which names will be resolved and displayed. If left absent, the root namespace will be used.E.g. base.List

- renderWidth
     - **Values**: *80, 100, 120*
     - **Description**: The preferred maximum line width (in characters) of the source code of definitions to be rendered. If left absent, the render width is assumed to be Width {widthToInt = 80}.


### Response:

- Status code 200
- Headers: [("Cache-Control","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /find

### GET Parameters:

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace root. If left absent, the most recent root will be used.

- relativeTo
     - **Values**: **
     - **Description**: The namespace relative to which names will be resolved and displayed. If left absent, the root namespace will be used.E.g. base.List

- limit
     - **Values**: *1, 10, 20*
     - **Description**: The maximum number of results to return. Defaults to 10.

- renderWidth
     - **Values**: *80, 100, 120*
     - **Description**: The preferred maximum line width (in characters) of the source code of definitions to be rendered. If left absent, the render width is assumed to be Width {widthToInt = 80}.

- query
     - **Values**: *foo, ff, td nr*
     - **Description**: Space-separated subsequences to find in the name of a type or term.


### Response:

- Status code 200
- Headers: [("Cache-Control","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

## GET /getDefinition

### GET Parameters:

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace root. If left absent, the most recent root will be used.

- relativeTo
     - **Values**: **
     - **Description**: The namespace relative to which names will be resolved and displayed. If left absent, the root namespace will be used.E.g. base.List

- names
     - **Values**: *.base.List, foo.bar, @abc123*
     - **Description**: A fully qualified name, hash-qualified name, or hash.
     - This parameter is a **list**. All GET parameters with the name names[] will forward their values in a list to the handler.

- renderWidth
     - **Values**: *80, 100, 120*
     - **Description**: The preferred maximum line width (in characters) of the source code of definitions to be rendered. If left absent, the render width is assumed to be Width {widthToInt = 80}.

- suffixifyBindings
     - **Values**: *True, False*
     - **Description**: If True or absent, renders definitions using the shortest unambiguous suffix. If False, uses the fully qualified name. 


### Response:

- Status code 200
- Headers: [("Cache-Control","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /list

### GET Parameters:

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace root. If left absent, the most recent root will be used.

- relativeTo
     - **Values**: **
     - **Description**: The namespace relative to which names will be resolved and displayed. If left absent, the root namespace will be used.E.g. base.List

- namespace
     - **Values**: **
     - **Description**: The namespace required by the endpoint.If left absent, the relativeTo namespace will be used.E.g. base.List


### Response:

- Status code 200
- Headers: [("Cache-Control","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- When no value is provided for `namespace`, the root namespace `.` is listed by default (`application/json;charset=utf-8`, `application/json`):

```javascript
{"namespaceListingFQN":".","namespaceListingHash":"#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5","namespaceListingChildren":[{"tag":"Subnamespace","contents":{"namespaceName":"base","namespaceHash":"#19d1o9hi5n642t8jttg","namespaceSize":237}}]}
```

## GET /namespaces/:namespace

### Captures:

- *namespace*: E.g. base.List

### GET Parameters:

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace root. If left absent, the most recent root will be used.

- renderWidth
     - **Values**: *80, 100, 120*
     - **Description**: The preferred maximum line width (in characters) of the source code of definitions to be rendered. If left absent, the render width is assumed to be Width {widthToInt = 80}.


### Response:

- Status code 200
- Headers: [("Cache-Control","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- When no value is provided for `namespace`, the root namespace `.` is listed by default (`application/json;charset=utf-8`, `application/json`):

```javascript
{"fqn":"","hash":"#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5","readme":null}
```

## GET /projects

### GET Parameters:

- rootBranch
     - **Values**: *#abc123*
     - **Description**: The hash or hash prefix of the namespace root. If left absent, the most recent root will be used.

- owner
     - **Values**: *unison, alice, bob*
     - **Description**: The name of a project owner


### Response:

- Status code 200
- Headers: [("Cache-Control","")]

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

```javascript
[]
```

- Projects in the root branch (`application/json;charset=utf-8`, `application/json`):

```javascript
[{"owner":"unison","name":"base","hash":"#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"}]
```

- Projects in the root branch, Projects in the root branch (`application/json;charset=utf-8`):

```javascript
[{"owner":"unison","name":"base","hash":"#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"},{"owner":"unison","name":"base","hash":"#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"}]
```

