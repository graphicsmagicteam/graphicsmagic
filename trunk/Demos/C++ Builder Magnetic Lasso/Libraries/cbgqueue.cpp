/* Extracted from glib\gqueue.c of GLib 2.12.1
 * Extracted by Ma Xiaoguang & Ma Xiaoming (gmbros@hotmail.com)
 * Date: 2011-02-18
 *
 * GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GQueue: Double ended queue implementation, piggy backed on GList.
 * Copyright (C) 1998 Tim Janik
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * MT safe
 */

#include <vcl.h>
#pragma hdrstop

#include <stdlib.h>

#include "cbgqueue.h"
#include "cbglist.h"


//------------------------------------------------------------------------------

/**
 * g_queue_new:
 *
 * Creates a new #GQueue. 
 *
 * Returns: a new #GQueue.
 **/
GQueue* g_queue_new(void)
{
  void *mem = malloc( sizeof(GQueue) );

  if (mem)
    memset(mem, 0, sizeof(GQueue));

  return (GQueue*)mem;
}
//------------------------------------------------------------------------------

/**
 * g_queue_free:
 * @queue: a #GQueue.
 *
 * Frees the memory allocated for the #GQueue.
 **/
void g_queue_free(GQueue *queue)
{
  if (queue == NULL)
    return;

  g_list_free(queue->head);
  free(queue);
}
//------------------------------------------------------------------------------

/**
 * g_queue_is_empty:
 * @queue: a #GQueue.
 *
 * Returns %TRUE if the queue is empty.
 *
 * Returns: %TRUE if the queue is empty.
 **/
bool g_queue_is_empty(GQueue *queue)
{
  if (queue == NULL)
    return TRUE;

  return (queue->head == NULL);
}
//------------------------------------------------------------------------------

/**
 * g_queue_push_tail:
 * @queue: a #GQueue.
 * @data: the data for the new element.
 *
 * Adds a new element at the tail of the queue.
 **/
void g_queue_push_tail(GQueue *queue, void *data)
{
  if (queue == NULL)
    return;

  queue->tail = g_list_append(queue->tail, data);
  
  if (queue->tail->next)
    queue->tail = queue->tail->next;
  else
    queue->head = queue->tail;

  queue->length++;
}
//------------------------------------------------------------------------------

/**
 * g_queue_pop_head:
 * @queue: a #GQueue.
 *
 * Removes the first element of the queue.
 *
 * Returns: the data of the first element in the queue, or %NULL if the queue
 *   is empty.
 **/
void* g_queue_pop_head(GQueue *queue)
{
  if (queue == NULL)
    return NULL;

  if (queue->head)
  {
    GList *node = queue->head;
    void  *data = node->data;

    queue->head = node->next;
    if (queue->head)
      queue->head->prev = NULL;
    else
      queue->tail = NULL;

    g_list_free(node);
    queue->length--;

    return data;
  }

  return NULL;
}
//------------------------------------------------------------------------------

/**
 * g_queue_peek_head:
 * @queue: a #GQueue.
 *
 * Returns the first element of the queue.
 *
 * Returns: the data of the first element in the queue, or %NULL if the queue
 *   is empty.
 **/
void* g_queue_peek_head(GQueue *queue)
{
  if (queue == NULL)
    return NULL;

  return queue->head ? queue->head->data : NULL;
}
//------------------------------------------------------------------------------

/**
 * g_queue_insert_before:
 * @queue: a #GQueue
 * @sibling: a #GList link that <emphasis>must</emphasis> be part of @queue
 * @data: the data to insert
 * 
 * Inserts @data into @queue before @sibling.
 *
 * @sibling must be part of @queue.
 * 
 * Since: 2.4
 **/
void g_queue_insert_before(GQueue *queue, GList *sibling, void *data)
{
  if (queue == NULL)
    return;

  if (sibling == NULL)
    return;

  queue->head = g_list_insert_before(queue->head, sibling, data);
  queue->length++;
}
//------------------------------------------------------------------------------

/**
 * g_queue_insert_after:
 * @queue: a #GQueue
 * @sibling: a #GList link that <emphasis>must</emphasis> be part of @queue
 * @data: the data to insert
 *
 * Inserts @data into @queue after @sibling
 *
 * @sibling must be part of @queue
 * 
 * Since: 2.4
 **/
void g_queue_insert_after(GQueue *queue, GList *sibling, void *data)
{
  if (queue == NULL)
    return;

  if (sibling == NULL)
    return;

  if (sibling == queue->tail)
    g_queue_push_tail(queue, data);
  else
    g_queue_insert_before(queue, sibling->next, data);
}
//------------------------------------------------------------------------------

/**
 * g_queue_peek_head_link:
 * @queue: a #GQueue
 * 
 * Returns the first link in @queue
 * 
 * Return value: the first link in @queue, or %NULL if @queue is empty
 * 
 * Since: 2.4
 **/
GList* g_queue_peek_head_link(GQueue *queue)
{
  if (queue == NULL)
    return NULL;

  return queue->head;
}
//------------------------------------------------------------------------------

/**
 * g_queue_peek_tail_link:
 * @queue: a #GQueue
 * 
 * Returns the last link @queue.
 * 
 * Return value: the last link in @queue, or %NULL if @queue is empty
 * 
 * Since: 2.4
 **/
GList* g_queue_peek_tail_link(GQueue *queue)
{
  if (queue == NULL)
    return NULL;

  return queue->tail;
}
//------------------------------------------------------------------------------
