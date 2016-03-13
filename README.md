##MacroCount

A Simple R Script to Count Your Macros

Version: **1.0**

Dependencies: Make Sure you have R installed

How to use:
-------------
No need to pass any arguments. Just follow the instructions in your console


```
setWeight()
```

Set your weight. Numerical Value only. Choose whether you want to use lbs or kgs and stick to it

```
setGoals()
```

Set your macro goals.

```
addFood()
```

Add Food to your Food Log. You need a nutrition label if you want to use this function. Once you add a food you have an option to save this food to your Food Database (foodDB). After you do this you do not need to enter the macro information any more

```
addFromDB()
```
Add Food to your Food Log directly from your Food Database. All the macro information is saved. So all you need to know is the Food ID (Row Number) and Portion Size (in either grams if you are using a scale or item numbers if not)

```
addToDatabase()
```

Add a Food to your Database (and NOT to your Log). You can later add this food from your database to your Food Log using "addFromDB"

```
predictByID()
```

This allows you to see how a food will affect your daily macros without actually adding it to your food log. Select your a food item from your database (row number) and enter your portion. You will get your current daily macro and goal daily macro. You can type "1" to change your portion size, if you find it acceptable type "2" to add it to your food log or "3" to exit

Everything is autosaved to "MacroCount.RData" in your Working Directory

The Module creates 4 Data.Frames - dailySummary, foodDB, foodLog and macroGoals. You can View them directly for more info 






