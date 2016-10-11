module Tests exposing (..)

import Test exposing (..)
import Expect
import Geocoding as G

apiKey = "ABCD"

googleKey = "AIzaSyAji7Gm0r66d9QfW1aPYGSocFlawSXNLMw"


all : Test
all =
    describe "A Test Suite"
        [ 
            testInitializeWithAddressOnly    
        ,   testInitializeWithComponentsOnly
        ,   testWithAddress  
        ,   testWithComponents  
        ,   testWithRegion
        ,   testWithLanguage
        ,   testWithBounds
        ]

testInitializeWithAddressOnly : Test
testInitializeWithAddressOnly =
    let
      request = G.requestForAddress "77 Battery St." apiKey
      expected = "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=77+Battery+St."
    in
        test "initializeWithAddressOnly" <|   
            \() -> Expect.equal (G.requestUrl request) expected

testInitializeWithComponentsOnly : Test
testInitializeWithComponentsOnly = 
    let     
        request = G.requestForComponents [("Spain", G.CountryComponent)] apiKey
        expected = "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&components=country%3ASpain"
    in
        test "initializeWithComponentsOnly" <|
            \() -> Expect.equal (G.requestUrl request) expected

testWithAddress : Test
testWithAddress = 
    let 
        request = G.requestForComponents [("Spain", G.CountryComponent)] apiKey
            |> G.withAddress("Toledo")
        expected = "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&components=country%3ASpain"
    in
        test "withAddress" <|
            \() -> Expect.equal (G.requestUrl request) expected

testWithComponents : Test
testWithComponents = 
    let 
        request = G.requestForAddress "Toledo" apiKey
            |> G.withComponent ("Spain", G.CountryComponent)
            |> G.withComponent ("Toledo", G.AdministrativeAreaComponent)
        expected = "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&components=administrative_area%3AToledo%7Ccountry%3ASpain"
    in
        test "withComponents" <|
            \() -> Expect.equal (G.requestUrl request) expected

testWithRegion : Test
testWithRegion = 
    let 
        request = G.requestForAddress "Toledo" apiKey            
            |> G.withRegion "ES"
        expected = "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&region=ES"
    in 
        test "withRegion" <|
            \() -> Expect.equal (G.requestUrl request) expected


testWithLanguage : Test
testWithLanguage = 
    let 
        request = G.requestForAddress "Toledo" apiKey            
            |> G.withLanguage "ES"
        expected = "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Toledo&language=ES"
    in 
        test "withLanguage" <|
            \() -> Expect.equal (G.requestUrl request) expected

testWithBounds : Test
testWithBounds = 
    let 
        request = G.requestForAddress "Belmont" apiKey          
            |> G.withBounds (41, -74) (42, -70)
        expected = "https://maps.googleapis.com/maps/api/geocode/json?key=ABCD&address=Belmont&bounds=41%2C-74%7C42%2C-70"
    in 
        test "withBounds" <|
            \() -> Expect.equal (G.requestUrl request) expected





