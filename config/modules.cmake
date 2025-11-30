# BITSET
option(STDLIB_BITSET "Does compile STDLIB BITSET" ON)

if(STDLIB_BITSET)
  message(STATUS "Enable stdlib bitset module")
else()
  message(STATUS "Disable stdlib bitset module")
  add_compile_definitions(STDLIB_NO_BITSET)
endif() 

# HASHMAP
option(STDLIB_HASHMAP "Does compile STDLIB HASHMAP" ON)

if(STDLIB_HASHMAP)
  message(STATUS "Enable stdlib hashmap module")
else()
  message(STATUS "Disable stdlib hashmap module")
  add_compile_definitions(STDLIB_NO_HASHMAP)
endif() 

