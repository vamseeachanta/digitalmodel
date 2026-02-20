The GHS API is very easy to use because it works with the same commands as GHS, so no manual is needed to use the API.
By using the GHS API in the Loading Condition Generator(LoadGen), it takes less than 10 minutes to create 10 loading conditions along with the stability check. Additionally, generating 23 consecutive loading conditions for offloading sequences takes less than 2 minutes.
However the GHS API requires separate license.

I have changed the interface of LoadGen to Excel. 
Not all features from the original program have been transferred yet, it can be used effectively as long as the data is updated. 
The main features are as below.
1. Ballasting/De-ballasting/Transfer ballast water while satisfying target values (Draft, LCG, TCG or Trim, Heel) 
2. Longitudinal strength control,
3. Slack tank control.
4. Exclude tanks from loading for inspection or maintenance conditions. 
5. Cargo loading can be done by inputting the loading percentage.
6. Easy to create topside module lifting conditions by selecting which items to exclude from the lightship category.
7. Generate GHS run file
8. Intact and Damage stability check(GHS API required)
9. It is possible to perform multiple loadings at once, but this functionality has not yet been transferred to Excel.
10. The main calculation module is hosted on Google server, so an internet connection is required to use LoadGen. Data is not stored, and when multiple users are using it simultaneously, it may return incorrect values.
11. Emergency Response feature will be added
12. LoadGen is available for the next 90 days, as the free usage period of the Google server is 90 days.
