# new elf every blank line
!NF { i++ }
# sum calories carried by each elf
{ elves[i] += $1 }
END { 
    # sort elves by calories carried
    n=asort(elves)
    # print the highest calorie elves
    for(i = 0; i < 3; i++) {
        sum += elves[n - i - 1] 
    }
    # print the sum of the highest calorie elves
    print sum
}
