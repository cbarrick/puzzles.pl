# [The Zebra Problem](http://en.wikipedia.org/wiki/Zebra_Puzzle)

> 1. There are five houses.
> 2. The Englishman lives in the red house.
> 3. The Spaniard owns the dog.
> 4. Coffee is drunk in the green house.
> 5. The Ukrainian drinks tea.
> 6. The green house is immediately to the right of the ivory house.
> 7. The Old Gold smoker owns snails.
> 8. Kools are smoked in the yellow house.
> 9. Milk is drunk in the middle house.
> 10. The Norwegian lives in the first house.
> 11. The man who smokes Chesterfields lives in the house next to the man with the fox.
> 12. Kools are smoked in the house next to the house where the horse is kept.
> 13. The Lucky Strike smoker drinks orange juice.
> 14. The Japanese smokes Parliaments.
> 15. The Norwegian lives next to the blue house.
>
> Now, who drinks water? Who owns the zebra?

— Life International, December 17, 1962


## zebra/1

`zebra(HOUSES)`

True if HOUSES is a list of houses satisfying the zebra problem. A house is a list: [Color, Nationality, Cigarete, Drink, Pet].


### Example

```prolog
?- zebra(HOUSES).
HOUSES = [
	[yellow, norwegian, kool,         water,        fox],
	[blue,   ukrainian, chesterfield, tea,          horse],
	[red,    english,   old-gold,     milk,         snails],
	[ivory,  spanish,   lucky-stripe, orange-juice, dog],
	[green,  japanese,  parliaments,  coffee,       zebra]
].
```


## Credits
This implementation is based on the SWI-Prolog benchmark for the same problem: https://github.com/SWI-Prolog/bench
