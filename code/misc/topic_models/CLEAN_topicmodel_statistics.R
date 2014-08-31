tp = topics(topic.models)
within_count=0
between_count=0
w_total=0
b_total = 0

w_ge90 = 0
b_ge90 =0
w_ge75 = 0
b_ge75 = 0
w_ge5 = 0
b_ge5 = 0

for(j in 1:6289) {
 
 base = tp[j]
 same_topic = setdiff(which(tp == base),j)
 diff_topic = setdiff(setdiff(1:6289, same_topic),j)

 within_count = within_count + length(same_topic)
 between_count = between_count + length(diff_topic)

 w_total = w_total + sum(total.lamb[j,same_topic])
 b_total = b_total + sum(total.lamb[j,diff_topic])

 w_ge90 = w_ge90 + sum(total.lamb[j,same_topic] >= .90)
 w_ge75 = w_ge75 + sum(total.lamb[j,same_topic] >= .75)
 w_ge5 = w_ge5 + sum(total.lamb[j,same_topic] >= .5)

 b_ge90 = b_ge90 + sum(total.lamb[j,diff_topic] >= .90)
 b_ge75 = b_ge75 + sum(total.lamb[j,diff_topic] >= .75)
 b_ge5 = b_ge5 + sum(total.lamb[j,diff_topic] >= .5)
print(j) 
}
