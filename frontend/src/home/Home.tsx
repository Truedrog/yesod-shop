import React, {useEffect} from 'react';

export default function Home() {
	
	useEffect(()=>{
		(async () => {
			console.log("TEST")
			await load()
			console.log("TEST after")
		})();
	},[])
	return (
		<div>
			<h2>Home</h2>
		</div>
	);
}

async function load(){
	try {
	  const t = await fetch("http://localhost:3000/api/data", {
		  mode: 'no-cors',
	  });
	  console.log(t);
	} catch(e){
		console.log(e)
	}

}
