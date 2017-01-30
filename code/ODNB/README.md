Summary
=======

The overall pipeline for going from raw ODNB data to models usuable in the portal is as follows:

1) text_processing
2) network_estimation
3) network_validation
4) network_processing

* text_processing/ Code for converting ODNB articles into document-count matrices. 

* network_estimation/ Code for performing network estimation. 

* network_validation/ Code for determining which method of network estimation is better.

* network_processing/ Code for converting network estimation results into web-interface stuff. This might not belong in the ODNB folder, since it is not ODNB specific, but I suppose it depends on where you want to take the code. 

Deprecated
==========

* obselete_code/ Old code, with some notes on what the contents are. There filenames should describe what is inside, but there are really no guarantees that any of this code works. I'm just keeping them around for historical reasons, and also for any emergency...

* unsorted_code/ There isn't anything in here that is essential to performing the previous tasks. These contain either older versions of things I've tried, miscellaneous experimental tasks, or just formatting data in certain ways. I'll process these last. 


