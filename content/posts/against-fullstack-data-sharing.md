---
title: "Against Fullstack Data Sharing"
date: 2020-02-13
description: "The unintuitive reasons why webapps written in a common language shouldn't share data formats"
draft: false
path: "/posts/against-fullstack-data-sharing"
type: post
image: "/print14.jpg"
tags:
  - "Programming"
  - "Frontend"
  - "Backend"
  - "Javascript"
  - "GraphQL"
---

This is a post about how I work with data in fullstack development. Specifically, I share what I think are
good patterns for sharing data and logic between the frontend and the backend of an application.

Initially, I was for sharing a lot of logic, classes, and data formats between a frontend and backend
written in the same language, but I've come to change my mind after trying it out on a real project.

# Fullstack what now?

As the title indicates, I'm against *Fullstack Data Sharing*, but what exactly do I mean by that?
I struggled to summarize the idea in a few words for the title, so let me give a more detailed
explanation of what I mean.

I like building apps on the web, using Javascript. Or rather, Typescript, but the process and libraries
are the same. One advantage of JS is that you can have the same language for the backend and
frontend of your application. The backend is the code that you, the developer, are responsible for
running, and the frontend is what you send to the users of your application. The backend code is
usually run on some kind of server, and most of its work centers around a database. The frontend
presents the functionality of the backend in a nice package for the user.

Anyways, you need to *do things* on both sides, and you always have common data between the sides.
For example, if I'm making a twitter clone, the concept of a "Tweet" exists on both the backend,
and the frontend. The backend will need to create and retrieve Tweets using the database,
and the frontend will need to display Tweets to the user, allow them to create Tweets, etc.
You need code to handle and manipulate this data on both sides, as well as ways to transfer
things between both sides.

## At least 2 serialization formats

Let's focus a little bit on *serialization*: transforming your code's representation of data
into something that can be sent over the wire, or stored in a database. 

There are 2 main places you need to serialize your data in a web application:

  - Moving data into the database
  - Moving data between the backend and the frontend


### JSON on the pipes

For moving data between the backend and the frontend, in 2020 the ubiquitous format is JSON. If it
were 2004, maybe I'd be saying XML instead, but those days are past us. Regardless of how you're
communicating with the backend, be it with GraphQL, REST, gRPC, or something else, you'll be using
JSON, or something like it.

JSON has maps, strings and lists, which can be used as the basic building blocks for pretty much
anything:

```json
{
  "type": "post",
  "data": {
    "created-at": "2020-02-12",
    "message": "Hello World!",
  }
  "replies": ["23424234", "3234234234"]
}
```

This is an example of a JSON structure, and it showcases how we can represent a lot of nifty things
with a relatively simple format. JSON is sufficient to represent a lot of our data structures.

Another advantage of JSON with JS, is that we can directly serialize (simple) JS objects to JSON:

```js
// {"foo": "bar"}
JSON.stringify({foo: "bar"})
```

there are some caveats with this approach I'll get into a bit later, but for now we'll put a tick
in the "advantages" column.

### ORMs in the back

So JSON's got us covered for sending things to the frontend, but what about communication between
the backend and our database? Well, this is where ORMs (usually) come in.

Essentially, an ORM (Object Relational Mapping) wraps the communication between our backend
and our database, by organizing  the functionality we need from our database around the *objects* in 
our application.

For example, instead of directly writing the SQL to create a Tweet, we'd have a Tweet *class*, with
a method to create a new Tweet as part of the class. We've moved the logic needed to interact
with the database into the objects we work with themselves.

Examples of ORMs in Typescript-land include:
  - [Mongoose](https://mongoosejs.com/) for MongoDB
  - [TypeORM](https://typeorm.io) for SQL like databases

For every sentence I could write about ORMs here, there's about 10 blog posts debating their merits;
I won't go into that here. Those blog posts do a much better job of illustrating the pros and cons
than I ever could in this post.

Now, what I do want to highlight about ORMs is that they impose at least one representation of data
onto you. Before having a database, I might have worked with Tweets using one class, or representation,
but if I want to now use TypeORM (for example), then I need to introduce a new representation
of Tweets, or tailor the existing one to much how TypeORM needs things to be.

Let's take one of the examples on TypeORM's homepage:

```ts
@Entity()
export class User {
    @PrimaryGeneratedColumn()
    id: number;

    @Column()
    firstName: string;

    @Column()
    lastName: string;

    @Column()
    age: number;
}
```

TypeORM requires us to create a *Model*, as they call it, which is the representation TypeORM prefers
for our data. Now, this library isn't being arbitrary in requiring us to annotate our class like this.
The reason TypeORM requires us to have `@Column` and `@PrimaryGeneratedColumn` annotations is
because it uses this to figure out how our Model maps onto our database.

When you use an ORM, you move the database-friendly representation of your data from the database
itself, into your application. In exchange, you don't need to convert from your application's
representation into your database's.

Without an ORM, you'd have the database's view of things, as defined by how your table is defined
in SQL, say:

```sql
CREATE TABLE User (
    id int primary key,
    firstName varchar(255),
    lastName varchar(255),
    age int
);
```

and then you'd have to write code to convert from what your application has defined a User to be,
into what the SQl says.

With an ORM, you have to define what a User is based on what your ORM requires, but it takes care
of the database for you.

### No ORM?

If you don't use an ORM, you still have a serialization format, since you need to convert your data
into something that the database can use, either by writing some SQL, in the case of a relational
database, or massaging it into the right document format, for the hip and cool NoSQL document
databases.

## Sharing in this Context

So you have some representation of your entity / concept in your application, be it in the form
of a class, an ORM model, an interface, a comment, etc. What I mean by sharing, is literally having
the code that defines this be used both in the backend of the application, and the frontend of
the application.

For example:

```typescript
// shared/tweet.ts
interface Tweet {
  id: string;
  content: string;
  authorID: string
}
```

And then `frontend/showtweet.ts` imports this file, and so does `backend/createtweet.ts`. So now
this code is shared between both the frontend and the backend.

### Advantages

This approach does have its merits, and at first I thought it was good as well. The main advantage
here is respecting the "Don't Repeat Yourself" principle.

A commonly heard mantra in programming is **DRY**, and using the same representation of data
on both the backend and the frontend is definitely respecting DRY, at least literally.
The alternative does involve repeating what you mean by a *Tweet*, or having two slightly
different concepts of a *Tweet*. This repetition can lead to problems if the two occurrences
grow out of sync.

### Disadvantages

The rest of this post is dedicated to explaining the disadvantages of sharing your data representations
verbatim between the frontend and the backend. I think there are enough disadvantages to warrant
avoiding this approach (at least in most cases), but I already announced that in the title, didn't I?

# Sharing your ORM model is "fun"

(And by "fun", I mean "painful", as usual)

The biggest issue with sharing your ORM model directly, at least with the ORMs I know of, is that
the ORM depends on the database. More specifically, an ORM library is going to have a dependency
on the database driver it wraps. The database driver is the library that provides a slight
abstraction over the network protocol the database communicates with. The database driver is probably
not something you want to be shipping to your user.

There's a common complaint going around that our applications, and especially those in the NPM
ecosystem, have gotten a bit too dependency-hungry. We have millions of dependencies, 8 layers deep,
and we can't stop adding more of them to our projects.

Illustrated:

![A diagram shows that .node_modules is heavier than a black hole](/node_modules_heavy.png)

One reason for this (I don't think I have the guts to pretend to diagnose the problem), is that
it's pretty easy to add a new dependency to an application, and hard to feel the costs.

Well, on the backend you don't feel the costs. On the frontend you actually try to care about
your bundle size, at least if you haven't given up hope yet. If your frontend loads quickly
and your bundle size is manageable, you try and avoid adding big libraries if it's not necessary.
You might even take pride in the size of your bundle.

With that in mind, importing the database driver into your frontend is probably not the best idea.

If you import the ORM model directly, you're depending on the model, and thus the ORM, and so you
need to bundle the database driver in there as well.

## Workarounds

Now, you can avoid bundling the database driver in the frontend, by using some kind of dummy version
of the annotations the ORM provides, which avoids actually generating the database methods for the model
you have.

The problem with this is that it kind of weakens the promise of sharing code between the backend
and the frontend. The promise is that with a shared library, you use it just like any other piece of
code, on both sides of your application. There's no need for conditional logic or fooling around
with advanced settings, it's just code you import directly.

If you now need to conditionally import a different version of the ORM in your frontend, you're
weakening the promise a bit. Now it's not as simple as it was before to import the shared code,
and you wonder if it might have been better to just duplicate a little bit of code instead
of trying to share those models.

## The wrong Model?

Another problem with using the database's representation in the frontend is a mismatch between
what the frontend uses, and what the database has.

There might be things in the database that never actually appear in the frontend. There are parts of
the database you might not use in *this specific* frontend application, if it's not the only
consumer of your database. There might also be details of you implement your API, such as
**Join Tables**, which shouldn't show up in your frontend.

You might also have some parts of the data you send to your frontend be calculated on the fly,
for example, counting the number of posts. This count doesn't appear in your ORM model, since it isn't
in the database, but rather queried on demand. But because you use this count as if it were a field
in the frontend, you now have a different representation of your data in your frontend.

If details like these leak through the ORM abstraction you have in the backend, then that's a sign
that you probably shouldn't be sharing these models directly with the frontend.

# Creating a shared class

"Okay, okay, I won't share the ORM model directly with the frontend"

Great! But we're not done yet, unfortunately. Let's say you've observed my previous gripes, and
even agreed with them, so now you separate things in two:

You have your ORM model, which you use for interacting with the database, and a new representation that
you share with both sides. This representation is probably a class, with some methods to do work
on the internal data.

## Serialization

This part is a bit Typescript specific.

If you have a simple Typescript interface, you can return that very easily from an API endpoint,
because it maps directly to JSON. Most frameworks will provide a way to just "send" the object
itself:

```js
app.get('/endpoint', (req, res) => {
  res.send({woah: 'mama'});
})
```

This is real convenient.

Now, if you have a class, you have a problem. You can't directly send the class as JSON, now you
need to write logic to serialize your class to JSON, and logic to parse your class back from the
JSON on the frontend.

If you try to just naively `JSON.stringify` it, you'll realize that the methods don't serialize
and can't be parsed back, so you need to always go through the class constructor on both ends.

To work around this, you can separate the "methods" of your class into functions, and then just
work with an interface, but that kind of defeats the point of using a class in the first place.

## Do you really need to share everything?

The serialization problems are more of an inconvenience. The biggest problem I have with this
"share" everything approach, is that you might be sharing things you don't need on the frontend,
or calculating things on the frontend you could have done in advance.

For example, some fields that are necessary in the backend might not be needed on the frontend. One
common example is the ubiquitous `id` field, On the backend you're going to be using this one a lot,
but on the frontend, you usually don't need the `id` of something, outside of the URL. Having
to send this each time is a bit of a waste.

The frontend usually wants *the result* of a method, and not the logic of method. For example,
if a user has an array of Tweets as part of their representation, the frontend may want to
know the count. They may want just the count, and not even the tweets themselves. This share
everything approach will send the tweets unnecessarily, and then make the frontend calculate
the count iself.

We could also include the count as part of the backend representation, but this becomes awkward
if we don't need the count for certain frontend things.

## Needs aren't uniform across the frontend.

We've talked about sharing code in a general sense, but we haven't really talked about the needs
of a frontend specifically. One aspect of a frontend you might not think about initially is how
the needs of your representation change based on which part of the frontend you're at. For example
if I'm displaying all the posts, I don't need their *content*, I just need the metadata. I only
need everything about the post on the page dedicated to that post.

If my API always returns everything about the post, that's quite a waste for the main page,
where I'm requesting all posts, but only using a fraction of the data for each post.

Because of these varying needs, there isn't a single representation of a concept on the frontend.
Rather, there's multiple *slices* of the same concept, which includes **all** the things you
might want to know about some entity. Ideally, you'd share this inclusive concept with the frontend,
and then allow it select which subset of information it needs for a specific part of the application.

# What do I advocate then?

In one word: [**GraphQL**](https://graphql.org/).

I've been hinting at it in the way I've described certain problems, and certain needs
the frontend has. In brief I think your "shared representation" should be the GraphQL structure
you provide, and the frontend can have as many slices of that representation as it needs. This
structure should not mirror the data you store, but rather provide both the data the frontend
might need, as well as the calculations it might want to do.

With GraphQL, you provide all the data and calculations the frontend might want, and then each
part of the frontend can make requests for exactly the parts they need. This prevents the oversharing
of unnecessary info, since each part of the frontend can just request the fields it needs.

This approach also helps avoid duplicate logic, by keeping it in the backend. Since a lot of the
methods we had previously have now become GraphQl fields, we no longer share or duplicate the
logic in the frontend. Instead, the frontend just sees the logic as if it were a static field,
which simplifies the work your frontend has to do. You can also take advantage of caching, to
avoid recalculating things. Caching class methods is possible, but a lot less convenient.

You're also free to handle things however you want on the backend. You just need to implement
your resolvers. The frontend doesn't need to know which fields are stored in the database,
which are calculated, and what's actually stored in the database. The backend and the frontend
are now very decoupled, but still share a nice source of truth. GraphQL completely eliminates
the problem of serialization across API boundaries, since it will massage things to and from
JSON for you.

## User-Centric Graph

You also want to avoid providing an all encompassing Graph. Instead, your graph should take a viewpoint
more specific to your application. For example, if you have a frontend application where users
interact with data specific to them, and don't have access to other user's data, then you should
provide a Graph that reflects that reality.

In this case, you'd have a field for the current user, rather than a field for all users, since
your frontend shouldn't be displaying all the users anyways.

Do:

```graphql
type Query {
  me: User!
}
```

Instead of:

```graphql
type Query {
  myUserID: ID!
  user($userID: ID!): User!
}
```

This does mean that you may end up with multiple GraphQL endpoints over a single database, which
is a good thing. You might want to provide one representation optimized for your frontend
application, and another for your analytics dashboard. This is normal, since the needs and access
restrictions of both applications are very different.

# Conclusion

I think you should make sharing classes and logic between the backend and frontend of your application
the exception, and not the the rule. The rule should be to share things by putting them into
your GraphQL representation. This representation acts both as a contract / source of truth between
both sides, will letting them be sufficiently decoupled.

Hopefully this post was useful, and I plan to supplement it with an example application, at some point.
