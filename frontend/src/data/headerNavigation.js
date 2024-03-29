export default [
    {
        title: 'Home',
        url: '/',
        // submenu: {
        //     type: 'menu',
        //     menu: [
        //         { title: 'Home 1', url: '/' },
        //         { title: 'Home 2', url: '/home-two' },
        //     ],
        // },
    },
    {
        title: 'Shop',
        url: '',
        submenu: {
            type: 'megamenu',
            menu: {
                size: 'nl',
                columns: [
                    {
                        size: 6,
                        links: [
                            {
                                title: 'Power Tools',
                                url: 'shop/category/1',
                                links: [
                                    { title: 'Engravers', url: '' },
                                    { title: 'Wrenches', url: '' },
                                    { title: 'Wall Chaser', url: '' },
                                    { title: 'Pneumatic Tools', url: '' },
                                ],
                            },
                            {
                                title: 'Machine Tools',
                                url: '',
                                links: [
                                    { title: 'Thread Cutting', url: '' },
                                    { title: 'Chip Blowers', url: '' },
                                    { title: 'Sharpening Machines', url: '' },
                                    { title: 'Pipe Cutters', url: '' },
                                    { title: 'Slotting machines', url: '' },
                                    { title: 'Lathes', url: '' },
                                ],
                            },
                        ],
                    },
                    {
                        size: 6,
                        links: [
                            {
                                title: 'Hand Tools',
                                url: '',
                                links: [
                                    { title: 'Screwdrivers', url: '' },
                                    { title: 'Handsaws', url: '' },
                                    { title: 'Knives', url: '' },
                                    { title: 'Axes', url: '' },
                                    { title: 'Multitools', url: '' },
                                    { title: 'Paint Tools', url: '' },
                                ],
                            },
                            {
                                title: 'Garden Equipment',
                                url: '',
                                links: [
                                    { title: 'Motor Pumps', url: '' },
                                    { title: 'Chainsaws', url: '' },
                                    { title: 'Electric Saws', url: '' },
                                    { title: 'Brush Cutters', url: '' },
                                ],
                            },
                        ],
                    },
                ],
            },
        },
    },
];
