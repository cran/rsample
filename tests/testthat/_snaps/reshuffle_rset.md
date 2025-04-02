# reshuffle_rset is working

    Code
      reshuffle_rset(rset_subclasses[[non_random_classes[[i]]]])
    Condition
      Warning:
      `reshuffle_rset()` will return an identical <rset> when called on <sliding_index> objects.
    Output
      # Sliding index resampling 
      # A tibble: 49 x 2
         splits        id     
         <list>        <chr>  
       1 <split [1/1]> Slice01
       2 <split [1/1]> Slice02
       3 <split [1/1]> Slice03
       4 <split [1/1]> Slice04
       5 <split [1/1]> Slice05
       6 <split [1/1]> Slice06
       7 <split [1/1]> Slice07
       8 <split [1/1]> Slice08
       9 <split [1/1]> Slice09
      10 <split [1/1]> Slice10
      # i 39 more rows

---

    Code
      reshuffle_rset(rset_subclasses[[non_random_classes[[i]]]])
    Condition
      Warning:
      `reshuffle_rset()` will return an identical <rset> when called on <sliding_period> objects.
    Output
      # Sliding period resampling 
      # A tibble: 7 x 2
        splits        id    
        <list>        <chr> 
      1 <split [7/7]> Slice1
      2 <split [7/7]> Slice2
      3 <split [7/7]> Slice3
      4 <split [7/7]> Slice4
      5 <split [7/7]> Slice5
      6 <split [7/7]> Slice6
      7 <split [7/1]> Slice7

---

    Code
      reshuffle_rset(rset_subclasses[[non_random_classes[[i]]]])
    Condition
      Warning:
      `reshuffle_rset()` will return an identical <rset> when called on <sliding_window> objects.
    Output
      # Sliding window resampling 
      # A tibble: 49 x 2
         splits        id     
         <list>        <chr>  
       1 <split [1/1]> Slice01
       2 <split [1/1]> Slice02
       3 <split [1/1]> Slice03
       4 <split [1/1]> Slice04
       5 <split [1/1]> Slice05
       6 <split [1/1]> Slice06
       7 <split [1/1]> Slice07
       8 <split [1/1]> Slice08
       9 <split [1/1]> Slice09
      10 <split [1/1]> Slice10
      # i 39 more rows

---

    Code
      reshuffle_rset(rset_subclasses[[non_random_classes[[i]]]])
    Condition
      Warning:
      `reshuffle_rset()` will return an identical <rset> when called on <rolling_origin> objects.
    Output
      # Rolling origin forecast resampling 
      # A tibble: 45 x 2
         splits         id     
         <list>         <chr>  
       1 <split [5/1]>  Slice01
       2 <split [6/1]>  Slice02
       3 <split [7/1]>  Slice03
       4 <split [8/1]>  Slice04
       5 <split [9/1]>  Slice05
       6 <split [10/1]> Slice06
       7 <split [11/1]> Slice07
       8 <split [12/1]> Slice08
       9 <split [13/1]> Slice09
      10 <split [14/1]> Slice10
      # i 35 more rows

---

    Code
      reshuffle_rset(rset_subclasses[[non_random_classes[[i]]]])
    Condition
      Warning:
      `reshuffle_rset()` will return an identical <rset> when called on <validation_time_split> objects.
    Output
      # Validation Set Split (0.75/0.25)  
      # A tibble: 1 x 2
        splits          id        
        <list>          <chr>     
      1 <split [37/13]> validation

---

    Code
      reshuffle_rset(rset_subclasses[[non_random_classes[[i]]]])
    Condition
      Warning:
      `reshuffle_rset()` will return an identical <rset> when called on <validation_set> objects.
    Output
      # A tibble: 1 x 2
        splits          id        
        <list>          <chr>     
      1 <split [30/10]> validation

---

    Code
      reshuffle_rset(resample)
    Condition
      Error in `reshuffle_rset()`:
      ! Cannot reshuffle this rset (`attr(rset, 'strata')` is "TRUE", not a column identifier)
      i If the original object was created with an older version of rsample, try recreating it with the newest version of the package.

---

    Code
      reshuffle_rset(rset_subclasses[["manual_rset"]])
    Condition
      Error in `reshuffle_rset()`:
      ! `manual_rset` objects cannot be reshuffled.

---

    Code
      reshuffle_rset(rset_subclasses[["manual_rset"]]$splits[[1]])
    Condition
      Error in `reshuffle_rset()`:
      ! `rset` must be an <rset> object.

