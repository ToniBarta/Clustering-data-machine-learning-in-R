

as.integer(max(intra_times$discoverability)  * 10 * max(intra_times$row_number))


rownames(intra_times[ intra_times_cluster$cluster == 3, ])





intra_days[intra_days_cluster$cluster== 1 , ]


as.integer(input$intra_days)

for ( i in 1: (length(intra_days_cluster$size))) {

	cluster_table = intra_days[intra_days_cluster$cluster== i , ]

	if (cluster_table == as.integer(input$intra_days){

		if ( any(cluster_table$user_id) == as.integer(input$user_id)){


			break
		}
	}


}


give user id: 36
Give time of day from 1 - 8: 2
Give what day is it from 1 -7: 4
Give what month is it from 1-12: 11
Give the location: Manchester
Give the temperature: 22


<hash> containing 6 key-value pair(s).
  intra_days : 501  41 206 443  48  36
  intra_locations :  36  44  48 219 348 362 403 413 443 444 449 456 458 460 468 477 478 480 482 488 494 495 501 502 518 519 521 525 535 540 541 542 543 544 545 546 547 548 549 550 552 553 554 555 566 569 576 578 593 599 605 610 612 617 618 619 620 621 622 627 633 634 647 676 725 778 779 786 792 884 938
  intra_months : 0
  intra_temperatures : 0
  intra_times : 593 694 201 527 535 456 453 962 410 696 636  36 451 791 500 225 782 554 558 142 477 633 655 594 424 478 588 698  41 516 699 531 454 439 710 717 409 785 767
  user_id : 36



[1] "index 1"
<hash> containing 4 key-value pair(s).
  intra_days : 501  41 206 443  48  36
  intra_locations :  36  44  48 219 348 362 403 413 443 444 449 456 458 460 468 477 478 480 482 488 494 495 501 502 518 519 521 525 535 540 541 542 543 544 545 546 547 548 549 550 552 553 554 555 566 569 576 578 593 599 605 610 612 617 618 619 620 621 622 627 633 634 647 676 725 778 779 786 792 884 938
  intra_times : 593 694 201 527 535 456 453 962 410 696 636  36 451 791 500 225 782 554 558 142 477 633 655 594 424 478 588 698  41 516 699 531 454 439 710 717 409 785 767
  user_id : 36



