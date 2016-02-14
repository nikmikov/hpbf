/**
 *   Datatypes used to represent road network output by hpbf compiler
 *   All data structures are designed for zero-copy  use from memory mapped files
 */

#ifndef __HPBF_DATATYPES_H__
#define __HPBF_DATATYPES_H__

#include <stdint.h>

#define HPBF_INVALID_ID 0xffffffff


// latitude and longitude packed in int32_t from floating point with precision .7
// to get floating point value: double val = latitude * 0.0000001
struct hpbf_point {
    int32_t latitude;
    int32_t longitude;
};


struct hpbf_node {
    struct hpbf_point coordinates;
    uint32_t links_index_begin;   // index of first link in hpbf_node_links[].
                                  // Array of hpbf_node size n, must contain 'stop'
                                  // node at the end to make iteration easier
};

struct hpbf_node_links {
    uint32_t link_id;
};


struct hpbf_link {
    uint32_t node_id_forward;  // destination node id when traverse in forward direction
    uint32_t node_id_backward; // destination node id when traverse in backward direction
    uint16_t length_m;
    uint16_t link_attribute_id;
};


enum hpbf_enum_road_class {
    ROAD_CLASS_MOTORWAY = 0,
    ROAD_CLASS_TRUNK = 1,
    ROAD_CLASS_PRIMARY = 2,
    ROAD_CLASS_SECONDARY = 3,
    ROAD_CLASS_TERTIARY = 4,
    ROAD_CLASS_UNCLASSIFIED = 5,
    ROAD_CLASS_RESIDENTIAL = 6,
    ROAD_CLASS_SERVICE = 7
};

struct hpbf_link_attributes {
    // flags
    uint32_t max_speed_kmh : 8; // if set to 0 then no special restriction (use
                                // default road class speed)
    uint32_t road_class : 3;
    uint32_t is_pedestrian_allowed : 1;
    uint32_t is_bicycle_allowed : 1;
    uint32_t is_car_allowed : 1;
    uint32_t is_unpaved : 1;
    uint32_t is_roundabout : 1;
    uint32_t is_ramp : 1;
};



#endif // __HPBF_DATATYPES_H__
