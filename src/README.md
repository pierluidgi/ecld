
  |------------|
  | Top        |
  | Supervisor |
  |------------|
    |
    |
  |--------------|
  | Domains      |
  | Controllers  |
  | Supervisor   |-----------------------\
  |--------------|                        \ about 100~200 domains per node
    |                                      \
|- -|- - - - - - - - - - - - - - |       | -\ - - - - - - - - 
    |                 DOMAIN                 \           DOMAIN N
| |--------------|               |       | |------------|
  | Domain       |                         | Domain     |
| | Controller   |               |       | | Controller |
  | Supervisor   |                         | Supervisor |
| |--------------|               |  ...  | |------------|
    |       \                              ...
|   |        \                   |
    |         \
|   |          \                 |
    |           \
|   |            \               |
    |             \
| |------------| |------------|  |
  | Domain     | | Domain     |
| | members    | | Controller |  |
  | Supervisor | | Gen-server |
| |------------| |------------|  |
    |       |
|   |       |                    |
  |----|  |----|
| | w1 |..| wN | Workers         |
  |----|  |----|
|- - - - - - - - - - - - - - - - |
                      
