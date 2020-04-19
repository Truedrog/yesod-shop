module Db.DbStuff (setupDb, cleanDb) where

import Database.Esqueleto hiding (from, on)
--import Database.Esqueleto.Experimental
import qualified Database.Esqueleto.PostgreSQL.JSON as PJSON
import Db.DbTypes
import Import hiding ((==.), (||.))

setupDb :: (MonadIO m) => SqlPersistT m ()
setupDb = do
  cleanDb
  -- populating power Tools category
  powerTools <- insert $ Category "Power Tools" Nothing
  drills <- insert $ Category {categoryTitle = "Drills", categoryParentId = Just powerTools}
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
                "Combi drill with soft-grip handle and work light. Compact and ergonomic. Full-metal chuck and brushless motor. Supplied with blow-moulded case. ",
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
                        value = "2-Speed  Variable & Reverse"
                      }
                  ]
          }
      ]

--  _ <- insert $ Category {categoryTitle = "Saws", categoryParentId = Just powerTools}
--  _ <- insert $ Category {categoryTitle = "Sanders", categoryParentId = Just powerTools}
-- popuplating power

-- populating hand tools cat
--  handTools <- insert $ Category "Hand Tools" Nothing
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
