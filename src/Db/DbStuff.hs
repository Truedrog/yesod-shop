module Db.DbStuff (setupDb, cleanDb) where

import Database.Esqueleto hiding (from, on)
import qualified Database.Esqueleto.PostgreSQL.JSON as PJSON
import Db.DbTypes
import Import hiding ((==.), (||.))

setupDb :: (MonadIO m) => SqlPersistT m ()
setupDb = do
  --  cleanDb
  -- populating power Tools category
  powerTools <- insert $ Category {categoryTitle = "Power Tools", categoryParentId = Nothing, categoryUrl = Just "/powertools"}
  drills <- insert $ Category {categoryTitle = "Drills", categoryParentId = Just powerTools, categoryUrl = Just "/drills"}
  -- populating drills
  void $
    insertMany_
      [ Product
          { productTitle = "DeWalt DCD778M2T-SFGB 18V 4.0Ah Li-Ion XR Brushless Cordless Combi Drill (979HF)",
            productCategory = drills,
            productDescription =
              Just
                "Ergonomic combi drill with 18V brushless motor and XR technology. \
                \  Features 13mm metal chuck, spindle lock, soft grip handle and LED light for workplace illumination. \
                \  Suitable for consistent screw driving into a variety of materials with different screw sizes. \
                \  Supplied with TSTAK compatible carry case and 2 x 4.0Ah batteries with battery life indicators.",
            productPrice = 149.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/DeWalt DCD778M2T-SFGB.jpg",
                    "images/products/DeWalt DCD778M2T-SFGB-1.jpg",
                    "images/products/DeWalt DCD778M2T-SFGB-2.jpg",
                    "images/products/DeWalt DCD778M2T-SFGB-3.jpg",
                    "images/products/DeWalt DCD778M2T-SFGB-4.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Battery Power",
                                value = "4.0 Ah"
                              },
                            ProductFeature
                              { featureName = "Manual or Powered",
                                value = "Powered"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Product Length",
                                value = "240 mm"
                              },
                            ProductFeature
                              { featureName = "Tool Weight with Battery",
                                value = "1.85 kg"
                              }
                          ]
                      }
                  ],
            productFeatures =
              Just $
                PJSON.JSONB
                  [ ProductFeature
                      { featureName = "Batteries",
                        value = "2 x 4.0Ah Li-Ion Batteries"
                      },
                    ProductFeature
                      { featureName = "Charge Time",
                        value = "100min Charge"
                      },
                    ProductFeature
                      { featureName = "Speeds",
                        value = "2-Speed"
                      }
                  ]
          },
        Product
          { productTitle = "Mac Allister MSCD18-Li-2 18V 1.5Ah Li-Ion Cordless Combi Drill",
            productCategory = drills,
            productDescription =
              Just
                "Combi drill with 21 torque settings for great versatility. Supplied with a handy nylon bag.",
            productPrice = 49.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/MSCD18-Li-2.jpg",
                    "images/products/MSCD18-Li-2-1.jpg",
                    "images/products/MSCD18-Li-3.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Battery Power",
                                value = "1.5 Ah"
                              },
                            ProductFeature
                              { featureName = "Charge Time",
                                value = "60min"
                              },
                            ProductFeature
                              { featureName = "Max. Torque",
                                value = "35Nm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Product Length",
                                value = "73 mm"
                              },
                            ProductFeature
                              { featureName = "Tool Weight with Battery",
                                value = "1.4 kg"
                              }
                          ]
                      }
                  ],
            productFeatures =
              Just $
                PJSON.JSONB
                  [ ProductFeature
                      { featureName = "Batteries",
                        value = "2 x 2.0Ah Li-Ion Batteries"
                      },
                    ProductFeature
                      { featureName = "Charge Time",
                        value = "60min Charge"
                      },
                    ProductFeature
                      { featureName = "Speeds",
                        value = "2-Speed  Variable & Reverse"
                      }
                  ]
          },
        Product
          { productTitle = "Bosch GSB 18 V-50 18V 2.0Ah Li-Ion Coolpack Brushless Cordless Combi Drill",
            productCategory = drills,
            productDescription =
              Just
                "Combi drill with soft-grip handle and work light. Compact and ergonomic. Full-metal chuck and brushless motor. Supplied with blow-moulded case.",
            productPrice = 119.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Bosch GSB 18 V-50.jpg",
                    "images/products/Bosch GSB 18 V-50-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Battery Power",
                                value = "2.0 Ah"
                              },
                            ProductFeature
                              { featureName = "Charge Time",
                                value = "60min"
                              },
                            ProductFeature
                              { featureName = "Max. Torque",
                                value = "50Nm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Product Length",
                                value = "191 mm"
                              },
                            ProductFeature
                              { featureName = "Tool Weight with Battery",
                                value = "1.4 kg"
                              }
                          ]
                      }
                  ],
            productFeatures =
              Just $
                PJSON.JSONB
                  [ ProductFeature
                      { featureName = "Batteries",
                        value = "2 x 2.0Ah Li-Ion Batteries"
                      },
                    ProductFeature
                      { featureName = "Charge Time",
                        value = "60min Charge"
                      },
                    ProductFeature
                      { featureName = "Speeds",
                        value = "2-Speed Variable & Reverse"
                      }
                  ]
          },
        Product
          { productTitle = "Erbauer ECDT18-Li-2 18V Li-Ion EXT Brushless Cordless Combi Drill - Bare ",
            productCategory = drills,
            productDescription =
              Just
                "Powerful and versatile combi drill with 24 torque settings, plus drill and hammer drill. Equipped with a brushless motor. Advanced brushless motor technology for more power and higher torque.",
            productPrice = 119.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Bosch GSB 18 V-50.jpg",
                    "images/products/Bosch GSB 18 V-50-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Battery Power",
                                value = "Does Not Include Battery or Charger"
                              },
                            ProductFeature
                              { featureName = "Max. Torque",
                                value = "120Nm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Product Length",
                                value = "203 mm"
                              },
                            ProductFeature
                              { featureName = "Tool Weight without Battery",
                                value = "1.1 kg"
                              }
                          ]
                      }
                  ],
            productFeatures =
              Just $
                PJSON.JSONB
                  [ ProductFeature
                      { featureName = "Speeds",
                        value = "2-Speed"
                      }
                  ]
          }
      ]
  saws <- insert $ Category {categoryTitle = "Saws", categoryParentId = Just powerTools, categoryUrl = Just "/saws"}
  void $
    insertMany_
      [ Product
          { productTitle = "Evolution R255SMS-DB 255mm Electric Double-Bevel Sliding Mitre Saw 230V",
            productCategory = saws,
            productDescription =
              Just
                "Uses a single blade to cut mild steel, non-ferrous metals, plastic and wood, even if nails are embedded in the material. Provides clean and precise cuts no matter the material. Offers a maximum cross cut of 300 x 80mm both ways.",
            productPrice = 199.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/evolution-r255sms-db-255mm.jpg",
                    "images/products/Bosch GSB 18 V-50-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Power",
                                value = "2000W"
                              },
                            ProductFeature
                              { featureName = "No Load Speed",
                                value = "2500 rpm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Blade Diameter",
                                value = "255 mm"
                              },
                            ProductFeature
                              { featureName = "Corded Tool Weight",
                                value = "18.5 kg"
                              }
                          ]
                      }
                  ],
            productFeatures =
              Just $
                PJSON.JSONB
                  [ ProductFeature
                      { featureName = "Switch",
                        value = "Lock-Off Switch"
                      }
                  ]
          },
        Product
          { productTitle = "DeWalt DCS391 165mm 18V Li-Ion XR Cordless Circular Saw - Bare",
            productCategory = saws,
            productDescription =
              Just
                "The DeWalt DCS391 XR circular saw is ideal for working on MDF, plywood and timber and features a 24-tooth TCT blade. The all-metal gearbox gives added durability, while the hex key lets you make any quick on-the-job tweaks.",
            productPrice = 109.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/DeWalt DCS391 165mm 18V.jpg",
                    "images/products/DeWalt DCS391 165mm 18V-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Max. Cutting Depth At 90deg",
                                value = "55 mm"
                              },
                            ProductFeature
                              { featureName = "No Load Speed",
                                value = "3700 rpm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Product Length",
                                value = "360 mm"
                              },
                            ProductFeature
                              { featureName = "Tool Weight without Battery",
                                value = "3.2 kg"
                              }
                          ]
                      }
                  ],
            productFeatures =
              Just $
                PJSON.JSONB
                  [ ProductFeature
                      { featureName = "Speeds",
                        value = "2-Speed"
                      },
                    ProductFeature
                      { featureName = "Blade",
                        value = "TCT Blade"
                      }
                  ]
          },
        Product
          { productTitle = "Evolution R185SMS 185mm Electric Single-Bevel Sliding Mitre Saw 240V ",
            productCategory = saws,
            productDescription =
              Just
                "Use a single blade to cut mild steel, non-ferrous metals, plastic and wood, even if nails are embedded in the material. Provides clean and precise cuts no matter the material. Durable die-cast aluminium base supports a variety of heavy duty materials.",
            productPrice = 99.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Evolution R185SMS 185mm.jpg",
                    "images/products/Evolution R185SMS 185mm-1.jpg",
                    "images/products/Evolution R185SMS 185mm-2.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Power",
                                value = "1000W"
                              },
                            ProductFeature
                              { featureName = "Max. Cutting Depth At 90deg",
                                value = "75 mm"
                              },
                            ProductFeature
                              { featureName = "No Load Speed",
                                value = "3700 rpm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Blade Diameter",
                                value = "185 mm"
                              },
                            ProductFeature
                              { featureName = "Corded Tool Weight",
                                value = "9.9 kg"
                              }
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          }
      ]
  sanders <- insert $ Category {categoryTitle = "Sanders", categoryParentId = Just powerTools, categoryUrl = Just "/sanders"}
  void $
    insertMany_
      [ Product
          { productTitle = "Mac Allister MSDLS160 160W Electric Detail Sander 220-240V",
            productCategory = sanders,
            productDescription =
              Just
                "Compact detail sander. Ideal for small surfaces and intricate sanding tasks around the home. Supplied with sanding paper, dust bag and vacuum adaptor.",
            productPrice = 19.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Mac Allister MSDLS160 160W.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Power",
                                value = "160W"
                              },
                            ProductFeature
                              { featureName = "Brushless",
                                value = "Brushed"
                              },
                            ProductFeature
                              { featureName = "Handle Type",
                                value = "Soft-Grip Handle"
                              },
                            ProductFeature
                              { featureName = "Orbits Per Minute",
                                value = "12,000 opm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Sanding Surface Length",
                                value = "147 mm"
                              },
                            ProductFeature
                              { featureName = "Sanding Surface Width",
                                value = "100 mm"
                              },
                            ProductFeature
                              { featureName = "Corded Tool Weight",
                                value = "1.5 kg"
                              }
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          },
        Product
          { productTitle = "Erbauer ERO450 150mm Electric Random Orbit Sander 220-240V",
            productCategory = sanders,
            productDescription =
              Just
                "Powerful orbit sander with rapid sheet-changing system and a cyclonic dust collection system for capturing and removing dust from air and work areas. Supplied with 3 x sanding sheets and a carry bag.",
            productPrice = 79.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Erbauer ERO450 150mm Electric.jpg",
                    "images/products/Erbauer ERO450 150mm Electric-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Power",
                                value = "450W"
                              },
                            ProductFeature
                              { featureName = "Brushless",
                                value = "Brushed"
                              },
                            ProductFeature
                              { featureName = "Handle Type",
                                value = "Rubber Overmould Grip"
                              },
                            ProductFeature
                              { featureName = "Orbits Per Minute",
                                value = "5000-12,000 opm"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Sanding Disc Diameter",
                                value = "150 mm"
                              }
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          },
        Product
          { productTitle = "Makita 9403 / 2 4'' Electric Belt Sander 240V",
            productCategory = sanders,
            productDescription =
              Just
                "Powerful, heavy duty belt sander. Suitable for sanding large surface areas. Features simple alignment and fast belt replacement for maximum efficiency. Supplied with abrasive belt, dust nozzle and dust collection bag.",
            productPrice = 79.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Erbauer ERO450 150mm Electric.jpg",
                    "images/products/Erbauer ERO450 150mm Electric-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Power",
                                value = "1200W"
                              },
                            ProductFeature
                              { featureName = "Brushless",
                                value = "Brushed"
                              },
                            ProductFeature
                              { featureName = "No Load Speed",
                                value = "500 m/min"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Sanding Surface Length",
                                value = "260 mm"
                              },
                            ProductFeature
                              { featureName = "Sanding Surface Width",
                                value = "100 mm"
                              },
                            ProductFeature
                              { featureName = "Product Length",
                                value = "406 mm"
                              },
                            ProductFeature
                              { featureName = "Corded Tool Weight",
                                value = "5.7 kg"
                              }
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          }
      ]
  polishes <- insert $ Category {categoryTitle = "Polishers", categoryParentId = Just powerTools, categoryUrl = Just "/polishers"}
  void $
    insertMany_
      [ Product
          { productTitle = "Milwaukee M12 BPS-421X 50mm 12V 2.0 & 4.0Ah Li-Ion RedLithium Cordless Sub-Compact Sander / Polisher",
            productCategory = polishes,
            productDescription =
              Just
                "High performance 4-pole motor which delivers up to 8300rpm for maximum productivity. RedLink overload protection and constant speed electronics deliver durability and performance. Supplied with Dynacase.",
            productPrice = 159.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Milwaukee M12 BPS-421X.jpg",
                    "images/products/Milwaukee M12 BPS-421X-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Batteries",
                                value = "1 x 2.0Ah & 1 x 4.0Ah Li-Ion Batteries"
                              },
                            ProductFeature
                              { featureName = "Charge Time",
                                value = "80min"
                              },
                            ProductFeature
                              { featureName = "Handle Type",
                                value = "Rubber Overmould Grip"
                              },
                            ProductFeature
                              { featureName = "Brushless",
                                value = "Brushed"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Sanding Disc Diameter",
                                value = "50 mm"
                              },
                            ProductFeature
                              { featureName = "Orbit Diameter",
                                value = "76 mm"
                              },
                            ProductFeature
                              { featureName = "Tool Weight with Battery",
                                value = "1.45 kg"
                              }
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          },
        Product
          { productTitle = "Makita PO6000C/2 Electric Random Orbital Polisher 240V",
            productCategory = polishes,
            productDescription =
              Just
                "Random orbit with forced rotation for scratch polishing and random orbit for final polishing or wax coating. Enhanced manoeuvrability with the switch being operable whilst the side handle is held and an easy-grip head enables sideway work as well as bonnet conditioning.",
            productPrice = 249.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Makita PO6000C Electric.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Handle Type",
                                value = "Ergonomic Grip"
                              },
                            ProductFeature
                              { featureName = "Power",
                                value = "900 W"
                              },
                            ProductFeature
                              { featureName = "Brushless",
                                value = "Brushed"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "Sanding Disc Diameter",
                                value = "50 mm"
                              },
                            ProductFeature
                              { featureName = "Orbit Diameter",
                                value = "76 mm"
                              },
                            ProductFeature
                              { featureName = "Corded Tool Weight",
                                value = "2.8 kg"
                              }
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          },
        Product
          { productTitle = "Flex PE8 Electric Rotary Polisher 240V",
            productCategory = polishes,
            productDescription =
              Just
                "Positive-drive orbital polisher with VR microprocessor control, constant speed control and a continuous speed control trigger. For added safety polisher features power failure restart protection, overload protection and temperature monitoring.",
            productPrice = 339.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/Flex PE8 Electric.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature
                              { featureName = "Handle Type",
                                value = "Ergonomic Handle"
                              },
                            ProductFeature
                              { featureName = "Power",
                                value = "900W"
                              },
                            ProductFeature
                              { featureName = "Brushless",
                                value = "Brushed"
                              }
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature
                              { featureName = "No Load Speed",
                                value = "160-480 rpm"
                              },
                            ProductFeature
                              { featureName = "Corded Tool Weight",
                                value = "2.6 kg"
                              }
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          }
      ]
  -- populating hand tools cat
  handTools <- insert $ Category {categoryTitle = "Hand Tools", categoryParentId = Nothing, categoryUrl = Just "/handtools"}
  pliersAndCutters <- insert $ Category {categoryTitle = "Pliers & Cutters", categoryParentId = Just handTools, categoryUrl = Just "/pliersncutters"}
  void $
    insertMany_
      [ Product
          { productTitle = "STANLEY FATMAX GROOVE JOINT PLIERS 10'' (250MM) (74176)",
            productCategory = pliersAndCutters,
            productDescription =
              Just
                "Versatile multi-grip pliers with induction-hardened jaws. Easy to use PushLock adjustment button.",
            productPrice = 9.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages =
              Just $
                PJSON.JSONB
                  [ "images/products/STANLEY FATMAX GROOVE JOINT.jpg",
                    "images/products/STANLEY FATMAX GROOVE JOINT-1.jpg"
                  ],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features =
                          [ ProductFeature {featureName = "Max. Jaw Opening", value = "62 mm"},
                            ProductFeature {featureName = "Manufacturer Guarantee", value = "1 Year Guarantee"}
                          ]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature {featureName = "Length", value = "250 mm"},
                            ProductFeature {featureName = "Total Product Weight", value = "0.362 kg"}
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          },
        Product
          { productTitle = "C.K AUTOMATIC WIRE STRIPPER (97204)",
            productCategory = pliersAndCutters,
            productDescription =
              Just
                "For stripping flat or round cable and wire. Features an integrated wire cutter with micro-adjustment knob, crimping jaws for insulated or non-insulated terminals and an integral stripping length stop for consistent results.",
            productPrice = 9.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages = Just $ PJSON.JSONB ["images/products/C.K AUTOMATIC WIRE STRIPPER.jpg"],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features = [ProductFeature {featureName = "Manufacturer Guarantee", value = "1 Year Guarantee"}]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature {featureName = "Length", value = "265 mm"},
                            ProductFeature {featureName = "Pliers/Cutters Overall Length (Band)", value = "200-299mm"},
                            ProductFeature {featureName = "Total Product Weight", value = "0.309 kg"}
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          },
        Product
          { productTitle = "STANLEY FATMAX AUTOMATIC WIRE STRIPPER (5934K)",
            productCategory = pliersAndCutters,
            productDescription =
              Just
                "Cuts, strips and crimps wires in seconds. Spring-loaded pliers can strip inner and outer sheaths on wires and cables 0.2-6mm. \
                \The pliers have crimping capability and are colour coded for crimps on common insulated and non-insulated terminals 1.5-6mm. \
                \Bi-material handles, allowing easy and comfortable grip.",
            productPrice = 19.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages = Just $ PJSON.JSONB ["images/products/STANLEY FATMAX AUTOMATIC WIRE STRIPPER (5934K).jpg", "STANLEY FATMAX AUTOMATIC WIRE STRIPPER (5934K)-1.jpg"],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features = [ProductFeature {featureName = "Manufacturer Guarantee", value = "1 Year Guarantee"}]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature {featureName = "Length", value = "203 mm"},
                            ProductFeature {featureName = "Pliers/Cutters Overall Length (Band)", value = "200-299mm"},
                            ProductFeature {featureName = "Total Product Weight", value = "0.3 kg"}
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          },
        Product
          { productTitle = "LOCKING PLIERS SET 3 PIECES",
            productCategory = pliersAndCutters,
            productDescription =
              Just
                "High grade heat-treated steel alloy construction. Classic trigger release designed to provide maximum locking force. Hardened teeth designed to grip from any angle.",
            productPrice = 8.99,
            productBadges = Just $ PJSON.JSONB ["new"],
            productImages = Just $ PJSON.JSONB ["images/products/LOCKING PLIERS SET 3 PIECES.jpg"],
            productRating = 5,
            productAvailability = True,
            productOptions = Just $ PJSON.JSONB [],
            productSpec =
              Just $
                PJSON.JSONB
                  [ ProductSpecType
                      { specName = "General",
                        features = [ProductFeature {featureName = "Manufacturer Guarantee", value = "1 Year Guarantee"}]
                      },
                    ProductSpecType
                      { specName = "Dimensions and Weight",
                        features =
                          [ ProductFeature {featureName = "Length", value = "127-229 mm"},
                            ProductFeature {featureName = "Pliers/Cutters Overall Length (Band)", value = "Mixed"},
                            ProductFeature {featureName = "Total Product Weight", value = "0.9 kg"}
                          ]
                      }
                  ],
            productFeatures = Just $ PJSON.JSONB []
          }
      ]

--  _ <- insert $ Category {categoryTitle = "Hand Tool Kits", categoryParentId = Just handTools}
--  _ <- insert $ Category {categoryTitle = "Sockets & Spanners", categoryParentId = Just handTools}
--  _ <- insert $ Category {categoryTitle = "Screwdrivers & Hex Keys", categoryParentId = Just handTools}
--  pure ()

cleanDb :: (MonadIO m) => SqlPersistT m ()
cleanDb = do
  truncateTable "product"
  resetSeq "product"
  truncateTable "category"
  resetSeq "category"
  where
    resetSeq tableName = rawExecute ("ALTER SEQUENCE " <> tableName <> "_id_seq RESTART WITH 1") []
    truncateTable tableName = rawExecute ("TRUNCATE TABLE " <> tableName <> " CASCADE") []
