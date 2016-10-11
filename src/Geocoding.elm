module Geocoding
    exposing
        ( send
        , requestForAddress
        , requestForComponents
        , withLanguage
        , withBounds
        , withRegion
        , withComponent
        , withAddress
        , GeocodingResult
        , Status
        , Viewport
        , ApiKey
        , Component(..)
        , LocationType(..)
        , ComponentType(..)
        , Response
        , requestUrl
        )

{-| This library is an interface to Google's geocoding service

https://developers.google.com/maps/documentation/geocoding/intro

It provides a pipline friendly, builder-like API, and ADTs that map as closely as possible to the Google API

You can start building a request one of two ways:

    Geocoding.requestForAddress "77 Battery St." apiKey
    Geocoding.requestForComponents [("Spain", G.CountryComponent)] apiKey

Once you've built your request, calling send will return a Task, which you Perform to generate your own msg types

# Types
@docs GeocodingResult, Status, Viewport, ApiKey, Component, LocationType, ComponentType, Response

# Building a request
@docs requestForAddress, requestForComponents, withAddress, withComponent, withLanguage, withRegion, withBounds

# Sending a request
@docs send

# Inspecting a request
@docs requestUrl
-}

import Dict exposing (Dict)
import Http
import String
import Task exposing (Task)
import Json.Decode.Pipeline
    exposing
        ( decode
        , required
        , optional
        , nullable
        )
import Json.Decode
    exposing
        ( list
        , int
        , string
        , succeed
        , fail
        , float
        , Decoder
        , (:=)
        )


-- Types


{-| response status and a list of results (list will be empty if status is other than OK)
https://developers.google.com/maps/documentation/geocoding/intro#GeocodingResponses
-}
type alias Response =
    { status : Status
    , results : List GeocodingResult
    }


{-| mapping of Google API response statuses
https://developers.google.com/maps/documentation/geocoding/intro#StatusCodes
-}
type Status
    = GeocodingOk
    | ZeroResults
    | OverQueryLimit
    | RequestDenied
    | InvalidRequest
    | UnknownError


{-| an individual result
https://developers.google.com/maps/documentation/geocoding/intro#Results
-}
type alias GeocodingResult =
    { addressComponents : List AddressComponent
    , formattedAddress : String
    , geometry : Geometry
    , types : List ComponentType
    , placeId : String
    }


{-| a component of an address
   https://developers.google.com/maps/documentation/geocoding/intro#Results
-}
type alias AddressComponent =
    { longName : Maybe String
    , shortName : Maybe String
    , types : List ComponentType
    }


{-| the latitude and longitude of the location, location type and recommended viewport
   https://developers.google.com/maps/documentation/geocoding/intro#Results
-}
type alias Geometry =
    { location : Location
    , locationType : LocationType
    , viewport : Viewport
    }


{-| address component types
   https://developers.google.com/maps/documentation/geocoding/intro#Types
-}
type ComponentType
    = StreetAddress
    | Route
    | Intersection
    | Political
    | Country
    | AdministrativeAreaLevel1
    | AdministrativeAreaLevel2
    | AdministrativeAreaLevel3
    | AdministrativeAreaLevel4
    | AdministrativeAreaLevel5
    | ColloquialArea
    | Locality
    | Sublocality
    | SublocalityLevel1
    | SublocalityLevel2
    | SublocalityLevel3
    | SublocalityLevel4
    | SublocalityLevel5
    | Neighborhood
    | Premise
    | Subpremise
    | PostalCode
    | NaturalFeature
    | Airport
    | Park
    | PostBox
    | StreetNumber
    | Floor
    | Room
    | Establishment
    | PointOfInterest
    | Parking
    | PostalTown
    | BusStation
    | TrainStation
    | TransitStation
    | PostalCodeSuffix
    | OtherComponent


type alias Location =
    { latitude : Float
    , longitude : Float
    }


{-| a bounding box
   https://developers.google.com/maps/documentation/geocoding/intro#Viewports
-}
type alias Viewport =
    { northeast : Location
    , southwest : Location
    }


{-| additional data about a location
   https://developers.google.com/maps/documentation/geocoding/intro#Result
-}
type LocationType
    = Rooftop
    | RangeInterpolated
    | GeometricCenter
    | Approximate



-- request types


type RequestInfo
    = Address String
    | Components (Dict String Component)
    | AddressAndComponents String (Dict String Component)


type alias GeocodingRequest =
    { requestInfo : RequestInfo
    , bounds : Maybe Viewport
    , language : Maybe String
    , region : Maybe String
    , apiKey : ApiKey
    }


{-| components for request filtering
   https://developers.google.com/maps/documentation/geocoding/intro#ComponentFiltering
-}
type Component
    = RouteComponent
    | LocalityComponent
    | AdministrativeAreaComponent
    | PostalCodeComponent
    | CountryComponent


{-| alias for a Google API key
-}
type alias ApiKey =
    String



-- Decoders


statusDecoder : Decoder Status
statusDecoder =
    string `Json.Decode.andThen` mapStatus


mapStatus : String -> Decoder Status
mapStatus str =
    case str of
        "OK" ->
            succeed GeocodingOk

        "ZERO_RESULTS" ->
            succeed ZeroResults

        "OVER_QUERY_LIMIT" ->
            succeed OverQueryLimit

        "REQUEST_DENIED" ->
            succeed RequestDenied

        "INVALID_REQUEST" ->
            succeed InvalidRequest

        "UNKNOWN_ERROR" ->
            succeed UnknownError

        _ ->
            fail "really, really, unknown error"


responseDecoder : Decoder Response
responseDecoder =
    decode Response
        |> required "status" statusDecoder
        |> required "results" resultListDecoder


resultListDecoder : Decoder (List GeocodingResult)
resultListDecoder =
    list resultDecoder


resultDecoder : Decoder GeocodingResult
resultDecoder =
    decode GeocodingResult
        |> required "address_components" addressComponentListDecoder
        |> required "formatted_address" string
        |> required "geometry" geometryDecoder
        |> required "types" typeListDecoder
        |> required "place_id" string


addressComponentListDecoder : Decoder (List AddressComponent)
addressComponentListDecoder =
    list addressComponentDecoder


addressComponentDecoder : Decoder AddressComponent
addressComponentDecoder =
    decode AddressComponent
        |> optional "long_name" (nullable string) Nothing
        |> optional "short_name" (nullable string) Nothing
        |> required "types" typeListDecoder


locationDecoder : Decoder Location
locationDecoder =
    decode Location
        |> required "lat" float
        |> required "lng" float


viewportDecoder : Decoder Viewport
viewportDecoder =
    decode Viewport
        |> required "northeast" locationDecoder
        |> required "southwest" locationDecoder


locationTypeDecoder : Decoder LocationType
locationTypeDecoder =
    string `Json.Decode.andThen` mapLocationType


geometryDecoder : Decoder Geometry
geometryDecoder =
    decode Geometry
        |> required "location" locationDecoder
        |> required "location_type" locationTypeDecoder
        |> required "viewport" viewportDecoder


typeListDecoder : Decoder (List ComponentType)
typeListDecoder =
    list typeDecoder


typeDecoder : Decoder ComponentType
typeDecoder =
    string `Json.Decode.andThen` mapComponentType


mapLocationType : String -> Decoder LocationType
mapLocationType str =
    case str of
        "ROOFTOP" ->
            succeed Rooftop

        "RANGE_INTERPOLATED" ->
            succeed RangeInterpolated

        "GEOMETRIC_CENTER" ->
            succeed GeometricCenter

        "APPROXIMATE" ->
            succeed Approximate

        _ ->
            fail "unknown location type"


mapComponentType : String -> Decoder ComponentType
mapComponentType str =
    case str of
        "street_address" ->
            succeed StreetAddress

        "route" ->
            succeed Route

        "intersection" ->
            succeed Intersection

        "political" ->
            succeed Political

        "country" ->
            succeed Country

        "administrative_area_level_1" ->
            succeed AdministrativeAreaLevel1

        "administrative_area_level_2" ->
            succeed AdministrativeAreaLevel2

        "administrative_area_level_3" ->
            succeed AdministrativeAreaLevel3

        "administrative_area_level_4" ->
            succeed AdministrativeAreaLevel4

        "administrative_area_level_5" ->
            succeed AdministrativeAreaLevel5

        "colloquial_area" ->
            succeed ColloquialArea

        "locality" ->
            succeed Locality

        "sublocality" ->
            succeed Sublocality

        "sublocality_level_1" ->
            succeed SublocalityLevel1

        "sublocality_level_2" ->
            succeed SublocalityLevel2

        "sublocality_level_3" ->
            succeed SublocalityLevel3

        "sublocality_level_4" ->
            succeed SublocalityLevel4

        "sublocality_level_5" ->
            succeed SublocalityLevel5

        "neighborhood" ->
            succeed Neighborhood

        "premise" ->
            succeed Premise

        "subpremise" ->
            succeed Subpremise

        "postal_code" ->
            succeed PostalCode

        "natural_feature" ->
            succeed NaturalFeature

        "airport" ->
            succeed Airport

        "park" ->
            succeed Park

        "post_box" ->
            succeed PostBox

        "street_number" ->
            succeed StreetNumber

        "floor" ->
            succeed Floor

        "room" ->
            succeed Room

        "establishment" ->
            succeed Establishment

        "point_of_interest" ->
            succeed PointOfInterest

        "parking" ->
            succeed Parking

        "postal_town" ->
            succeed PostalTown

        "bus_station" ->
            succeed BusStation

        "train_station" ->
            succeed TrainStation

        "transit_station" ->
            succeed TransitStation

        "postal_code_suffix" ->
            succeed PostalCodeSuffix

        _ ->
            succeed OtherComponent



-- URL building


geocodingUrl : String
geocodingUrl =
    "https://maps.googleapis.com/maps/api/geocode/json"


{-| for inspecting the request URL for testing purposes
-}
requestUrl : GeocodingRequest -> String
requestUrl =
    Http.url geocodingUrl << toParameters



-- query formatting


componentToString : Component -> String
componentToString c =
    case c of
        RouteComponent ->
            "route"

        LocalityComponent ->
            "locality"

        AdministrativeAreaComponent ->
            "administrative_area"

        PostalCodeComponent ->
            "postal_code"

        CountryComponent ->
            "country"


locationToString : Location -> String
locationToString l =
    String.join "," [ toString l.latitude, toString l.longitude ]


viewportToString : Viewport -> String
viewportToString v =
    String.join "|" [ locationToString v.southwest, locationToString v.northeast ]


componentsToString : Dict String Component -> String
componentsToString components =
    String.join ("|")
        <| Dict.foldr (\k v acc -> acc ++ [ componentToString v ++ ":" ++ k ]) [] components


requestInfoParameter : RequestInfo -> List ( String, String )
requestInfoParameter info =
    case info of
        Address a ->
            [ ( "address", a ) ]

        Components c ->
            [ ( "components", componentsToString c ) ]

        AddressAndComponents a c ->
            [ ( "address", a ), ( "components", componentsToString c ) ]


singleton : a -> List a
singleton x =
    [ x ]


toParameters : GeocodingRequest -> List ( String, String )
toParameters req =
    List.concat
        [ [ ( "key", req.apiKey ) ]
        , requestInfoParameter req.requestInfo
        , Maybe.map (singleton << (,) "bounds" << viewportToString) req.bounds |> Maybe.withDefault []
        , Maybe.map (singleton << (,) "language") req.language |> Maybe.withDefault []
        , Maybe.map (singleton << (,) "region") req.region |> Maybe.withDefault []
        ]


{-| transform a request into a Task

    Geocoding.requestForAddress "77 Battery St" apiKey
      |> Geocoding.send
      |> Task.perform MyFailureMsg MySuccessMsg
-}
send : GeocodingRequest -> Task Http.Error Response
send req =
    Http.get responseDecoder <| requestUrl req


{-| Build a request for an address

    Geocoding.requestForAddress "77 Battery St" apiKey
-}
requestForAddress : String -> ApiKey -> GeocodingRequest
requestForAddress address =
    GeocodingRequest (Address address) Nothing Nothing Nothing


{-| Build a request for a list of component filters

    Geocoding.requestForComponents [("Spain", Geocoding.CountryComponent), ("Toledo", Geocoding.AdministrativeAreaComponent)] apiKey
-}
requestForComponents : List ( String, Component ) -> ApiKey -> GeocodingRequest
requestForComponents components =
    GeocodingRequest (Components <| Dict.fromList components) Nothing Nothing Nothing


{-| Specify the language for the request

    Geocoding.requestForAddress "77 Battery St" apiKey
      |> Geocoding.withLanguage("FR")
-}
withLanguage : String -> GeocodingRequest -> GeocodingRequest
withLanguage lang { requestInfo, bounds, language, region, apiKey } =
    GeocodingRequest requestInfo bounds (Just lang) region apiKey


{-| Specify a viewport bias for the request

    Geocoding.requestForAddress "Belmont" apiKey
      |> Geocoding.withBounds (41, -74) (42, -70)
-}
withBounds : ( Float, Float ) -> ( Float, Float ) -> GeocodingRequest -> GeocodingRequest
withBounds ( swLat, swLng ) ( neLat, neLng ) { requestInfo, bounds, language, region, apiKey } =
    let
        viewport =
            Viewport (Location neLat neLng) (Location swLat swLng)
    in
        GeocodingRequest requestInfo (Just viewport) language region apiKey


{-| specify region biasing for request

    Geocoding.requestForAddress "Toledo" apiKey
      |> Geocoding.withRegion "ES"
-}
withRegion : String -> GeocodingRequest -> GeocodingRequest
withRegion reg { requestInfo, bounds, language, region, apiKey } =
    GeocodingRequest requestInfo bounds language (Just reg) apiKey


{-| add a component filter to a request (can be called more than once for a request)

    Geocoding.requestForAddress "Toledo" apiKey
      |> Geocoding.withComponent ("Spain", Geocoding.CountryComponent)
-}
withComponent : ( String, Component ) -> GeocodingRequest -> GeocodingRequest
withComponent comp { requestInfo, bounds, language, region, apiKey } =
    let
        info =
            requestInfo |> addComponent comp
    in
        GeocodingRequest info bounds language region apiKey


{-| set the address to a request. If called more than once, the later call overwrites the earlier

    Geocoding.requestForComponents [("Spain", Geocoding.CountryComponent), ("Toledo", Geocoding.AdministrativeAreaComponent)] apiKey
      |> Geocoding.withAddress "Toledo"
-}
withAddress : String -> GeocodingRequest -> GeocodingRequest
withAddress address { requestInfo, bounds, language, region, apiKey } =
    let
        info =
            requestInfo |> addAddress address
    in
        GeocodingRequest info bounds language region apiKey



-- request building helpers


addAddress : String -> RequestInfo -> RequestInfo
addAddress address req =
    case req of
        Address old ->
            Address address

        Components dict ->
            AddressAndComponents address dict

        AddressAndComponents old dict ->
            AddressAndComponents address dict


addComponent : ( String, Component ) -> RequestInfo -> RequestInfo
addComponent ( val, comp ) req =
    case req of
        Address address ->
            AddressAndComponents address <| Dict.fromList [ ( val, comp ) ]

        Components dict ->
            Components <| Dict.insert val comp dict

        AddressAndComponents address dict ->
            AddressAndComponents address <| Dict.insert val comp dict
