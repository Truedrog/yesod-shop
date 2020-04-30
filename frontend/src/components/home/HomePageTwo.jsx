// react
import React, {useEffect, useState} from 'react';
// third-party
import {Helmet} from 'react-helmet-async';
// blocks
import BlockBanner from '../blocks/BlockBanner';
import BlockCategories from '../blocks/BlockCategories';
import BlockProductColumns from '../blocks/BlockProductColumns';
import BlockProducts from '../blocks/BlockProducts';
import BlockSlideShow from '../blocks/BlockSlideShow';
import BlockTabbedProductsCarousel from '../blocks/BlockTabbedProductsCarousel';
// data stubs
import categories from '../../data/shopBlockCategories';
import theme from '../../data/theme';
import {connect} from "react-redux";


function HomePageTwo(props) {
    const {products} = props;

    const columns = [
        {
            title: 'Top Rated Products',
            products: products.items.slice(0, 3),
        },
        {
            title: 'Special Offers',
            products: products.items.slice(3, 6),
        },
        {
            title: 'Bestsellers',
            products: products.items.slice(6, 9),
        },
    ];

    if(products.loading) {
        return <div>Loading...</div>
    }

    console.log(products)
    return (
        <React.Fragment>
            <Helmet>
                <title>{`Home Page Two — ${theme.name}`}</title>
            </Helmet>

            <BlockSlideShow/>

            {/*<BlockFeatures layout="boxed" />*/}

            <BlockTabbedProductsCarousel products={products} title="Featured Products" layout="grid-5" rows={2} />

            <BlockBanner/>

            <BlockProducts
                title="Bestsellers"
                layout="large-last"
                featuredProduct={products.items[0]}
                products={products.items.slice(1, 7)}
            />

            <BlockCategories title="Popular Categories" layout="compact" categories={categories}/>

            {/*<BlockTabbedProductsCarousel title="New Arrivals" layout="grid-5" />*/}

            {/*<BlockPosts title="Latest News" layout="grid-nl" posts={posts} />*/}

            {/*<BlockBrands />*/}

            <BlockProductColumns columns={columns}/>
        </React.Fragment>
    );
}
const mapStateToProps = (state) => ({
    products: state.products
});

const mapDispatchToProps = {
};

export default connect(mapStateToProps, mapDispatchToProps)(HomePageTwo);
